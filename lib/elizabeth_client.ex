defmodule Elizabeth.Client do
  use GenServer.Behaviour

  defrecord ClientInfo, [:server, :reader, :sock, :nick]

  def start_link(server, sock) do
    :gen_server.start_link(__MODULE__, [server, sock], [])
  end

  def init([server, sock]) do
    :erlang.process_flag(:trap_exit, true)

    sock.send "Nick: \r\n"

    pid = Process.self
    reader = spawn_link fn() ->
      main_loop(pid, sock)
    end

    { :ok, ClientInfo.new(server: server, reader: reader, sock: sock) }
  end

  def handle_call({ :receive, nick }, from, state = ClientInfo[nick: nil]) do
    :gen_server.reply from, :ok

    nick = String.strip(nick)
    case :gen_server.call(state.server, { :join, nick }) do
      :ok -> { :noreply, state.nick(nick) }
      _ ->   { :noreply, state }
    end
  end

  def handle_call({ :receive, msg }, from, state) do
    :gen_server.reply from, :ok

    :gen_server.call state.server, { :say, msg }
    { :noreply, state }
  end

  def handle_cast({ :message, msg }, state) do
    state.sock.send msg
    { :noreply, state }
  end

  def handle_info({:EXIT, pid, _}, state) do
    if state.reader == pid do
      :gen_server.call state.server, :leave
      { :stop, :normal, state }
    else
      { :noreply, state }
    end
  end

  def terminate(reason, state) do
    try do
      :erlang.exit(state.reader, reason)
    catch
      _ -> :ok
    end
  end

  defp main_loop(server, sock) do
    case sock.recv(0) do
      { :ok, data } ->
        :gen_server.call server, { :receive, data }
        main_loop(server, sock)

      { :error, :closed } ->
        :ok
    end
  end
end
