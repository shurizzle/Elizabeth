defmodule Elizabeth.Server do
  use GenServer.Behaviour

  defrecord ServerInfo, [:reader, :clients]

  def t2c(trans) do
    case trans do
      :tcp -> TCPSocket
      :ssl -> SSLSocket
      _ -> nil
    end
  end

  def start_link({transport, port}) do
    :error_logger.info_msg("Starting server on port #{inspect port}\r\n")
    { :ok, sock } = case t2c(transport) do
                      nil -> nil
                      x -> x.listen(port, [:binary, {:packet, :line}, {:active, false}, {:reuseaddr, true}])
                    end
    :gen_server.start_link(__MODULE__, sock, [])
  end

  def init(sock) do
    :erlang.process_flag(:trap_exit, true)

    pid = Process.self
    reader = spawn_link fn() ->
      main_loop(pid, sock)
    end

    { :ok, ServerInfo.new(reader: reader, clients: []) }
  end

  def handle_call({ :join, nick }, {from,_}, state) do
    :erlang.link(from)

    case List.keyfind(state.clients, from, 1) do
      { nick, ^from } -> { :reply, false, state }

      _ ->
        :error_logger.info_msg "Join #{nick}\r\n"
        broadcast state.clients, "User #{nick} join\r\n"
        new_state = state.clients([{nick, from}|state.clients])
        { :reply, :ok, new_state }
    end
  end

  def handle_call({:say, msg}, {from,_}, state) do
    case List.keyfind(state.clients, from, 1) do
      { nick, _ } ->
        :error_logger.info_msg nick <> " sends: " <> msg
        broadcast state.clients, from, nick <> ": " <> msg

      _ -> :ok
    end
    { :reply, :ok, state }
  end

  def handle_call(:leave, {from,_}, state) do
    { :reply, :ok, leave(from, state) }
  end

  def handle_info({:EXIT, pid, _}, state) do
    if pid == state.reader do
      { :stop, :normal, state }
    else
      { :noreply, leave(pid, state) }
    end
  end

  def terminate(reason, state) do
    :erlang.exit state.reader, reason
  end

  defp broadcast(clients, msg) do
    Enum.each clients, fn({_, pid}) ->
      :gen_server.cast pid, { :message, msg }
    end
  end

  defp broadcast(clients, sender, msg) do
    Enum.each clients, fn({_, pid}) ->
      if pid != sender do
        :gen_server.cast(pid, { :message, msg })
      end
    end
  end

  defp leave(who, state) do
    case List.keyfind(state.clients, who, 1) do
      { nick, _ } ->
        :error_logger.info_msg "#{inspect who} leave\r\n"
        new_state = state.clients(List.keydelete state.clients, who, 1)
        broadcast new_state.clients, "User #{nick} left\r\n"
        new_state
      _ -> state
    end
  end

  defp main_loop(server, sock) do
    { :ok, csock } = sock.accept
    Elizabeth.Client.start_link server, csock
    main_loop server, sock
  end
end
