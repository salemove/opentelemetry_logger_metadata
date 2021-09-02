defmodule OpentelemetryLoggerMetadata.MixProject do
  use Mix.Project

  def project do
    [
      app: :opentelemetry_logger_metadata,
      version: "0.1.0-rc",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: [
        main: "OpentelemetryLoggerMetadata",
        extras: ["README.md"]
      ],
      elixirc_paths: elixirc_paths(Mix.env()),
      package: [
        name: "opentelemetry_logger_metadata",
        description: "Adds OpenTelemetry trace identifiers to logs",
        maintainers: ["Glia TechMovers"],
        licenses: ["MIT"],
        links: %{
          "GitHub" => "https://github.com/salemove/opentelemetry_logger_metadata",
          "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
          "OpenTelemetry.io" => "https://opentelemetry.io"
        },
        files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*)
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:opentelemetry_api, "~> 1.0.0-rc"},
      {:opentelemetry, "~> 1.0.0-rc", only: [:test]},
      {:logstash_logger_formatter, "~> 1.1", only: [:test]},
      {:jason, "~> 1.0", only: [:test]},
      {:ex_doc, "~> 0.24", only: [:dev], runtime: false}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end
