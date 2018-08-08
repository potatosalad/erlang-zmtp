defmodule ZMTP.MixProject do
  use Mix.Project

  def project() do
    [
      app: :zmtp,
      version: "0.0.0",
      elixir: "~> 1.7",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      name: "zmtp",
      package: package(),
      source_url: "https://github.com/potatosalad/erlang-zmtp"
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application() do
    [
      extra_applications: []
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    []
  end

  defp description() do
    """
    ZeroMQ Message Transport Protocol (ZMTP) for Erlang and Elixir
    """
  end

  defp package() do
    [
      name: :zmtp,
      files: [
        "CHANGELOG*",
        "include",
        "lib",
        "LICENSE*",
        "mix.exs",
        "priv",
        "README*",
        "rebar.config",
        "src"
      ],
      licenses: ["Mozilla Public License Version 2.0"],
      links: %{
        "GitHub" => "https://github.com/potatosalad/erlang-zmtp"
      },
      maintainers: ["Andrew Bennett"]
    ]
  end
end
