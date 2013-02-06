defprotocol Socket do
  Kernel.defmacro __using__(_) do
    quote location: :keep do
      defdelegate [accept(sock), accept(timeout, sock), recv(length, sock), recv(length, timeout, sock),
        send(data, sock), close(sock), peername(sock), port(sock), sockname(sock), getopts(options, sock),
        setopts(options, sock), controlling_process(new_owner, sock), shutdown(how, sock),
        format(format, sock), format(format, args, sock)], to: Socket, append_first: true
    end
  end

  def accept(sock)
  def accept(sock, timeout)
  def recv(sock, length)
  def recv(sock, length, timeout)
  def send(sock, data)
  def close(sock)
  def peername(sock)
  def port(sock)
  def sockname(sock)
  def getopts(sock, options)
  def setopts(sock, options)
  def controlling_process(sock, new_owner)
  def shutdown(sock, how)

  Kernel.def format(sock, format) do
    sock.format(format, [])
  end

  Kernel.def format(sock, format, args) do
    sock.send(:io_lib.format(format, args))
  end
end

defrecord TCPSocket, [:sock] do
  use Socket

  defp transform({ :ok, sock }) do
    { :ok, TCPSocket.new(sock: sock) }
  end

  defp transform(error) do
    error
  end

  def connect(address, port, options) do
    transform(:gen_tcp.connect(address, port, options))
  end

  def connect(address, port, options, timeout)
    when is_integer(timeout) or timeout == :infinity do
    transform(:gen_tcp.connect(address, port, options, timeout))
  end

  def listen(port, opts) do
    transform(:gen_tcp.listen(port, opts))
  end
end

defimpl Socket, for: TCPSocket do
  defp transform({ :ok, sock }) do
    { :ok, TCPSocket.new(sock: sock) }
  end

  defp transform(error) do
    error
  end

  def accept(sock) do
    Socket.accept(sock, :infinity)
  end

  def accept(sock, timeout) do
    transform(:gen_tcp.accept(sock.sock, timeout))
  end

  def recv(sock, length) do
    :gen_tcp.recv(sock.sock, length)
  end

  def recv(sock, length, timeout) do
    :gen_tcp.recv(sock.sock, length, timeout)
  end

  def send(sock, data) do
    :gen_tcp.send(sock.sock, data)
  end

  def close(sock) do
    :gen_tcp.close(sock.sock)
  end

  def peername(sock) do
    :inet.peername(sock.sock)
  end

  def port(sock) do
    :inet.port(sock.sock)
  end

  def sockname(sock) do
    Socket.peername(sock)
  end

  def getopts(sock, options) do
    :inet.getopts(sock.sock, options)
  end

  def setopts(sock, options) do
    :inet.setopts(sock.sock, options)
  end

  def controlling_process(sock, new_owner) do
    :gen_tcp.controlling_process(sock.sock, new_owner)
  end

  def shutdown(sock, how) do
    :gen_tcp.shutdown(sock.sock, how)
  end
end

defrecord SSLSocket, [:sock] do
  use Socket

  defp ensure_ssl() do
    case :ssl.start() do
      :ok -> :ok
      { :error, { :already_started, :ssl } } -> :ok
      error -> error
    end
  end

  defp transform({ :ok, sock }) do
    { :ok, SSLSocket.new(sock: sock) }
  end

  defp transform(error) do
    error
  end

  def connect(address, port, options) do
    case ensure_ssl() do
      :ok -> transform(:ssl.connect(address, port, options))
      error -> error
    end
  end

  def connect(address, port, options, timeout)
    when is_integer(timeout) or timeout == :infinity do
    case ensure_ssl() do
      :ok -> transform(:ssl.connect(address, port, options, timeout))
      error -> error
    end
  end

  def listen(port, opts) do
    case ensure_ssl() do
      :ok -> transform(:ssl.listen(port, opts))
      error -> error
    end
  end
end

defimpl Socket, for: SSLSocket do
  defp transform({ :ok, sock }) do
    { :ok, SSLSocket.new(sock: sock) }
  end

  defp transform(error) do
    error
  end

  def accept(sock) do
    Socket.accept(sock, :infinity)
  end

  def accept(sock, timeout) do
    case :ssl.transport_accept(sock.sock, timeout) do
      { :ok, new_socket } ->
        transform(:ssl.ssl_accept(new_socket))

      error -> error
    end
  end

  def recv(sock, length) do
    :ssl.recv(sock.sock, length)
  end

  def recv(sock, length, timeout) do
    :ssl.recv(sock.sock, length, timeout)
  end

  def send(sock, data) do
    :ssl.send(sock.sock, data)
  end

  def close(sock) do
    :ssl.close(sock.sock)
  end

  def peername(sock) do
    :ssl.peername(sock.sock)
  end

  def port(sock) do
    case :ssl.peername(sock.sock) do
      { :ok, { _, port } } -> { :ok, port }
      error -> error
    end
  end

  def sockname(sock) do
    Socket.peername(sock)
  end

  def getopts(sock, options) do
    :ssl.getopts(sock.sock, options)
  end

  def setopts(sock, options) do
    :ssl.setopts(sock.sock, options)
  end

  def controlling_process(sock, new_owner) do
    :ssl.controlling_process(sock.sock, new_owner)
  end

  def shutdown(sock, how) do
    :ssl.shutdown(sock.sock, how)
  end
end
