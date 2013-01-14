defmodule Socket do
  defp ensure_ssl() do
    case :ssl.start() do
      :ok -> :ok
      { :error, { :already_started, :ssl } } -> :ok
      error -> error
    end
  end

  defp transform({ :ok, sock }) do
    { :ok, { Socket, sock } }
  end

  defp transform(error) do
    error
  end

  def connect(address, port, options) do
    transform(:gen_tcp.connect(address, port, options))
  end

  def connect(address, port, options, false) do
    connect(address, port, options)
  end

  def connect(address, port, options, :tcp) do
    connect(address, port, options)
  end

  def connect(address, port, options, true) do
    connect(address, port, options, :ssl)
  end

  def connect(address, port, options, :ssl) do
    transform(:ssl.connect(address, port, options))
  end

  def connect(address, port, options, timeout)
    when is_integer(timeout) or timeout == :infinity do
    transform(:gen_tcp.connect(address, port, options, timeout))
  end

  def connect(address, port, options, timeout, false) do
    connect(address, port, options, timeout)
  end

  def connect(address, port, options, timeout, :tcp) do
    connect(address, port, options, timeout)
  end

  def connect(address, port, options, timeout, true) do
    connect(address, port, options, timeout, :ssl)
  end

  def connect(address, port, options, timeout, :ssl)
    when is_integer(timeout) or timeout == :infinity do
    transform(:ssl.connect(address, port, options, timeout))
  end

  def listen(port, opts) do
    transform(:gen_tcp.listen(port, opts))
  end

  def listen(port, opts, false) do
    listen(port, opts)
  end

  def listen(port, opts, :tcp) do
    listen(port, opts)
  end

  def listen(port, opts, true) do
    listen(port, opts, :ssl)
  end

  def listen(port, opts, :ssl) do
    case ensure_ssl() do
      :ok -> transform(:ssl.listen(port, opts))
      error -> error
    end
  end

  def accept(sock = { Socket, _ }) do
    sock.accept(:infinity)
  end

  def accept(timeout, { Socket, sock }) when is_port(sock) do
    transform(:gen_tcp.accept(sock, timeout))
  end

  def accept(timeout, { Socket, sock = { :sslsocket, _ } }) do
    case :ssl.transport_accept(sock, timeout) do
      { :ok, new_socket } ->
        transform(:ssl.ssl_accept(new_socket))

      error -> error
    end
  end

  def recv(length, { Socket, sock }) when is_port(sock) do
    :gen_tcp.recv(sock, length)
  end

  def recv(length, { Socket, sock = { :sslsocket, _ } }) do
    :ssl.recv(sock, length)
  end

  def recv(length, timeout, { Socket, sock }) when is_port(sock) do
    :gen_tcp.recv(sock, length, timeout)
  end

  def recv(length, timeout, { Socket, sock = { :sslsocket, _ } }) do
    :ssl.recv(sock, length, timeout)
  end

  def send(data, { Socket, sock }) when is_port(sock) do
    :gen_tcp.send(sock, data)
  end

  def send(data, { Socket, sock = { :sslsocket, _ } }) do
    :ssl.send(sock, data)
  end

  def close({ Socket, sock }) when is_port(sock) do
    :gen_tcp.close(sock)
  end

  def close({ Socket, sock = { :sslsocket, _ } }) do
    :ssl.close(sock)
  end

  def peername({ Socket, sock }) when is_port(sock) do
    :inet.peername(sock)
  end

  def peername({ Socket, sock = { :sslsocket, _ } }) do
    :ssl.peername(sock)
  end

  def port({ Socket, sock }) when is_port(sock) do
    :inet.port(sock)
  end

  def port({ Socket, sock = { :sslsocket, _ } }) do
    case :ssl.peername(sock) do
      { :ok, { _, port } } -> { :ok, port }
      error -> error
    end
  end

  def sockname(sock) do
    peername(sock)
  end

  def getopts(options, { Socket, sock }) when is_port(sock) do
    :inet.getopts(sock, options)
  end

  def getopts(options, { Socket, sock = { :sslsocket, _ } }) do
    :ssl.getopts(sock, options)
  end

  def setopts(options, { Socket, sock }) when is_port(sock) do
    :inet.setopts(sock, options)
  end

  def setopts(options, { Socket, sock = { :sslsocket, _ } }) do
    :ssl.setopts(sock, options)
  end

  def controlling_process(new_owner, { Socket, sock }) when is_port(sock) do
    :gen_tcp.controlling_process(sock, new_owner)
  end

  def controlling_process(new_owner, { Socket, sock = { :sslsocket, _ } }) do
    :ssl.shutdown(sock, new_owner)
  end

  def shutdown(how, { Socket, sock }) when is_port(sock) do
    :gen_tcp.shutdown(sock, how)
  end

  def shutdown(how, { Socket, sock = { :sslsocket, _ } }) do
    :ssl.shutdown(sock, how)
  end

  def format(format, sock = { Socket, _ }) do
    sock.format(format, [])
  end

  def format(format, args, sock = { Socket, _ }) do
    sock.send(:io_lib.format(format, args))
  end
end
