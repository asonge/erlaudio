defmodule Mix.Tasks.Compile.Erlaudio do
  @shortdoc "Compiles Erlaudio"

  def run(_) do
    {result, _error_code} = if match? {:win32, _}, :os.type do
      System.cmd("nmake", ["/F", "Makefile.win", "priv\\erlaudio_drv.dll"], stderr_to_stdout: true)
    else
      System.cmd("make", ["priv/erlaudio_drv.so"], stderr_to_stdout: true)
    end
    Mix.shell.info result

    :ok
  end

  def clean() do
    {result, _error_code} = if match? {:win32, _}, :os.type do
      System.cmd("nmake", ["/F", "Makefile.win", "clean"], stderr_to_stdout: true)
    else
      System.cmd("make", ["clean"], stderr_to_stdout: true)
    end
    Mix.shell.info result
  end

end

defmodule Erlaudio.Mixfile do
  use Mix.Project

  def project do
    [app: :erlaudio,
    description: "Erlang audio bindings to portaudio",
    package: package,
    version: "0.2.0",
    language: :erlang,
    compilers: [:erlaudio, :erlang, :app],
    # aliases: [clean: ["clean", "clean.erlaudio"]],
    deps: deps,
    escript: [main_module: :erlaudio_escript, language: :erlang, app: nil]]
  end

  def application do
    [applications: [], mod: []]
  end

  defp deps do
    []
  end

  defp package do
    %{
      contributors: ["Alex Songe"],
      files: ~w"c_src include priv src Makefile Makefile.win README* CHANGELOG* LICENSE*",
      licenses: ["Apache 2"],
      links: %{"Github" => "https://github.com/asonge/erlaudio"}
    }
  end
end
