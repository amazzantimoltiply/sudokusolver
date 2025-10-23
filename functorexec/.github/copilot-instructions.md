# Copilot Instructions for functorexec

## Project Overview
This is a minimal Haskell project using Stack for build management. The project follows standard Haskell conventions with a library-executable structure and Stack's LTS resolver system.

## Build System & Workflows
- **Build**: Use `stack build` (or VS Code task "haskell build")
- **Test**: Use `stack test` (or VS Code task "haskell test")  
- **Clean build**: Use `stack clean && stack build` (or VS Code task "haskell clean & build")
- **Watch mode**: Use `stack build --test --no-run-tests --file-watch` for continuous building
- **Execute**: `stack exec functorexec-exe` runs the compiled executable

## Project Structure Conventions
- `src/Lib.hs` - Main library module with exposed functions
- `app/Main.hs` - Executable entry point that imports and uses library functions
- `test/Spec.hs` - Test suite entry point (currently placeholder)
- `package.yaml` - **Primary** build configuration (Hpack format)
- `functorexec.cabal` - **Generated** from package.yaml (do not edit manually)
- `stack.yaml` - Stack resolver and project configuration

## Development Patterns
- The project uses Stack's LTS 24.16 resolver for reproducible builds
- Library code goes in `src/`, executable code in `app/`
- The executable imports the library via `import Lib` 
- GHC warnings are comprehensive: includes `-Wall`, `-Wcompat`, and specific incomplete pattern warnings
- Threading is enabled for executables with `-threaded -rtsopts -with-rtsopts=-N`

## Key Dependencies & Configuration
- Base package constrained to `>= 4.7 && < 5` for GHC compatibility
- No external dependencies beyond base (minimal project)
- Uses Hpack for package.yaml → .cabal file generation
- Stack configuration uses remote snapshot from commercialhaskell/stackage-snapshots

## AI Agent Guidelines
- Always use Stack commands rather than Cabal directly
- When adding dependencies, modify `package.yaml`, not the .cabal file
- Run `stack build` after package.yaml changes to regenerate .cabal
- Use VS Code tasks for common operations when available
- The project currently has minimal functionality - `someFunc` just prints a message
- Test suite is not implemented yet - `test/Spec.hs` is a placeholder