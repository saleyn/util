defmodule CompileTime do
  @doc"""
  Evaluate a function at compile time.

  ## Example
    iex> require CompileTime
    iex> CompileTime.eval(&NaiveDateTime.utc_now/0)
    %NaiveDateTime{}
  """
  defmacro eval(f) do
    {fun, _} = Code.eval_quoted(f)
    val = fun.()

    quote do
      unquote(Macro.escape(val))
    end
  end
end
