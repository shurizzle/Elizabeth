defmodule Elizabeth.Sup do
  @behavior :supervisor

  def start_link(args) do
    :supervisor.start_link(__MODULE__, args)
  end

  def init(args) do
    tree = [ { Elizabeth.Server,
      { Elizabeth.Server, :start_link, args },
      :permanent, 5000, :worker, [ Elizabeth.Server ] } ]
    { :ok, { { :one_for_one, 5, 5 }, tree } }
  end
end
