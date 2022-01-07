defmodule OpentelemetryLoggerMetadata.MixProject do
  use Mix.Project

  def project do
    {app, desc} = load_app()
    config = load_config()

    [
      app: app,
      version: version(Keyword.fetch!(desc, :vsn)),
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(Keyword.fetch!(config, :deps)),
      docs: [
        main: "OpentelemetryLoggerMetadata",
        extras: ["README.md"]
      ],
      elixirc_paths: elixirc_paths(Mix.env()),
      package: [
        name: "opentelemetry_logger_metadata",
        description: to_string(Keyword.fetch!(desc, :description)),
        maintainers: ["Glia TechMovers"],
        licenses: ["MIT"],
        links: %{
          "GitHub" => "https://github.com/salemove/opentelemetry_logger_metadata",
          "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
          "OpenTelemetry.io" => "https://opentelemetry.io"
        },
        files:
          ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG* rebar.config rebar.lock VERSION src)
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp version({:file, path}) do
    path
    |> File.read!()
    |> String.trim()
  end

  defp deps(rebar) do
    rebar
    |> Enum.map(fn {dep, version} -> {dep, to_string(version)} end)
    |> Enum.concat([
      {:opentelemetry, "~> 1.0", only: [:test]},
      {:logstash_logger_formatter, "~> 1.1", only: [:test]},
      {:jason, "~> 1.0", only: [:test]},
      {:ex_doc, "~> 0.24", only: [:dev], runtime: false}
    ])
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp load_config do
    {:ok, config} = :file.consult("rebar.config")

    config
  end

  defp load_app do
    {:ok, [{:application, name, desc}]} =
      :file.consult("src/opentelemetry_logger_metadata.app.src")

    {name, desc}
  end
end
