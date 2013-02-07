defmodule Elizabeth.Server do
  use GenServer.Behaviour

  defrecord ServerInfo, [:readers, :clients]

  def t2c(trans) do
    case trans do
      :tcp -> TCPSocket
      :ssl -> SSLSocket
      _ -> nil
    end
  end

  def start_link(ports) do
    :gen_server.start_link(__MODULE__, ports, [])
  end

  def init(ports) do
    :erlang.process_flag(:trap_exit, true)

    ports = :lists.filter(fn
      ({:tcp, port}) when is_integer(port) -> true
      ({:ssl, port, _}) when is_integer(port) -> true
      (_) -> false
    end, ports)

    pid = Process.self
    readers = Enum.map(ports, fn ({trans, port}) ->
        { spawn_link(fn() ->
          main_loop(pid, trans, port)
        end), trans, port }
      ({trans, port, cert}) ->
        { spawn_link(fn() ->
          main_loop(pid, trans, port, cert)
        end), trans, port, cert }
    end)

    { :ok, ServerInfo.new(readers: readers, clients: []) }
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
    case List.keyfind(state.readers, pid, 0) do
      { ^pid, trans, port } ->
        cpid = Process.self

        state = state.readers(List.keyreplace(state.readers, pid, 0,
          { spawn_link(fn() ->
            main_loop(cpid, trans, port)
          end), trans, port }))

        { :noreply, state }

      { ^pid, trans, port, cert } ->
        cpid = Process.self

        state = state.readers(List.keyreplace(state.readers, pid, 0,
          { spawn_link(fn() ->
            main_loop(cpid, trans, port, cert)
          end), trans, port, cert }))

        { :noreply, state }

      _ ->
        { :noreply, leave(pid, state) }
    end
  end

  def terminate(reason, state) do
    Enum.map(state.readers, fn({ reader, _, _ }) ->
      :erlang.exit reader, reason
    end)
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

  defp main_loop(server, trans, port) do
    main_loop(server, trans, port, 5000)
  end

  defp main_loop(server, :ssl, port, cert) do
    main_loop(server, :ssl, port, cert, 5000)
  end

  defp main_loop(server, :tcp, port, sleep_for) do
    :error_logger.info_msg("Starting server on port #{inspect port}\r\n")
    try do
      { :ok, sock } = TCPSocket.listen(port, [:binary, {:packet, :line}, {:active, false}, {:reuseaddr, true}])

      :error_logger.info_msg "#{inspect Process.self} started for {:tcp, #{inspect port}}"
      main_loop2(server, sock)
    catch
      _ -> :timer.sleep(sleep_for)
    end
  end

  defp main_loop(server, :ssl, port, cert, sleep_for) do
    :error_logger.info_msg("Starting server on port #{inspect port}\r\n")
    try do
      { :ok, sock } = SSLSocket.listen(port, [:binary, {:packet, :line}, {:active, false}, {:reuseaddr, true}, {:certfile, cert}, {:verify, :verify_none}])

      :error_logger.info_msg "#{inspect Process.self} started for {:ssl, #{inspect port}}"
      main_loop2(server, sock)
    catch
      _ -> :timer.sleep(sleep_for)
    end
  end

  defp main_loop2(server, sock) do
    x = sock.accept
    :error_logger.info_msg inspect x
    { :ok, csock } = x
    case Elizabeth.Client.start_link(server, csock) do
      { :ok, pid } ->
        csock.controlling_process pid
      _ -> nil
    end
    main_loop2 server, sock
  end
end
