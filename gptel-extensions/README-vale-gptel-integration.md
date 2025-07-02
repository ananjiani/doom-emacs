# Vale-GPTel Integration

This integration allows gptel-rewrite to use vale-ls diagnostics as context when rewriting text.

## Features

- Extracts vale-ls errors from the selected region
- Passes these errors as context to gptel when rewriting
- Automatically includes all vale diagnostics in the rewrite instruction

## Usage

1. Select a region of text that has vale-ls errors
2. Use `SPC r v` (`my/gptel-rewrite-with-vale`) - Rewrite with vale errors as context

## How It Works

1. **Diagnostic Extraction**: The integration uses LSP functions to get vale-ls diagnostics:
   - `lsp--get-buffer-diagnostics` retrieves all diagnostics
   - Filters for diagnostics within the selected region
   - Only includes diagnostics from "vale-ls" source

2. **Context Formatting**: Vale errors are formatted as a list:
   ```
   The text has the following writing issues identified by vale:
   - Line X: Error message
   - Line Y: Another error message
   ```

3. **Integration with gptel-rewrite**:
   - Modifies `gptel--rewrite-message` to include vale context
   - Calls the standard `gptel-rewrite` function with enhanced context

## File Structure

- `gptel-vale-integration.el` - Main integration code
- `config.el` - Loads the integration and sets up keybindings
- `test-vale-gptel.org` - Test file with example text containing errors

## Requirements

- gptel package
- lsp-mode with vale-ls configured
- vale-ls server running

## Customization

You can customize the behavior by:

1. Modifying `my/format-vale-diagnostics-for-prompt` to change how errors are formatted
2. Adjusting `my/vale-rewrite-directive` to change the system message
3. Creating additional functions with different prompting strategies