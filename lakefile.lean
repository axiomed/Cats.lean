import Lake
open Lake DSL

package «Cats» where
  -- add package configuration options here

lean_lib «Cats» where
  -- add library configuration options here

lean_exe «tests» where
  root := `Tests
  supportInterpreter := true
