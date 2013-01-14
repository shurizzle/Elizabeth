defmodule :elizabeth do
  @behavior :application

  def start(_type, port) do
    { :ok, pid } = Elizabeth.start(port)
    :erlang.register(:elizabeth, pid)
    { :ok, pid }
  end

  def stop(_) do
    Elizabeth.stop(:erlang.whereis(:elizabeth))
  end
end

defmodule Elizabeth do
  def start(port) do
    Elizabeth.Sup.start_link([port])
  end

  def stop(pid) do
    :erlang.exit(pid, :kill)
  end
end
