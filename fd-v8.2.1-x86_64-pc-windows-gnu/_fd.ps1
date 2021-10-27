
using namespace System.Management.Automation
using namespace System.Management.Automation.Language

Register-ArgumentCompleter -Native -CommandName 'fd' -ScriptBlock {
    param($wordToComplete, $commandAst, $cursorPosition)

    $commandElements = $commandAst.CommandElements
    $command = @(
        'fd'
        for ($i = 1; $i -lt $commandElements.Count; $i++) {
            $element = $commandElements[$i]
            if ($element -isnot [StringConstantExpressionAst] -or
                $element.StringConstantType -ne [StringConstantType]::BareWord -or
                $element.Value.StartsWith('-')) {
                break
        }
        $element.Value
    }) -join ';'

    $completions = @(switch ($command) {
        'fd' {
            [CompletionResult]::new('-d', 'd', [CompletionResultType]::ParameterName, 'Set maximum search depth (default: none)')
            [CompletionResult]::new('--max-depth', 'max-depth', [CompletionResultType]::ParameterName, 'Set maximum search depth (default: none)')
            [CompletionResult]::new('--maxdepth', 'maxdepth', [CompletionResultType]::ParameterName, 'maxdepth')
            [CompletionResult]::new('--min-depth', 'min-depth', [CompletionResultType]::ParameterName, 'min-depth')
            [CompletionResult]::new('--exact-depth', 'exact-depth', [CompletionResultType]::ParameterName, 'exact-depth')
            [CompletionResult]::new('-t', 't', [CompletionResultType]::ParameterName, 'Filter by type: file (f), directory (d), symlink (l),
executable (x), empty (e), socket (s), pipe (p)')
            [CompletionResult]::new('--type', 'type', [CompletionResultType]::ParameterName, 'Filter by type: file (f), directory (d), symlink (l),
executable (x), empty (e), socket (s), pipe (p)')
            [CompletionResult]::new('-e', 'e', [CompletionResultType]::ParameterName, 'Filter by file extension')
            [CompletionResult]::new('--extension', 'extension', [CompletionResultType]::ParameterName, 'Filter by file extension')
            [CompletionResult]::new('-x', 'x', [CompletionResultType]::ParameterName, 'Execute a command for each search result')
            [CompletionResult]::new('--exec', 'exec', [CompletionResultType]::ParameterName, 'Execute a command for each search result')
            [CompletionResult]::new('-X', 'X', [CompletionResultType]::ParameterName, 'Execute a command with all search results at once')
            [CompletionResult]::new('--exec-batch', 'exec-batch', [CompletionResultType]::ParameterName, 'Execute a command with all search results at once')
            [CompletionResult]::new('-E', 'E', [CompletionResultType]::ParameterName, 'Exclude entries that match the given glob pattern')
            [CompletionResult]::new('--exclude', 'exclude', [CompletionResultType]::ParameterName, 'Exclude entries that match the given glob pattern')
            [CompletionResult]::new('--ignore-file', 'ignore-file', [CompletionResultType]::ParameterName, 'ignore-file')
            [CompletionResult]::new('-c', 'c', [CompletionResultType]::ParameterName, 'When to use colors: never, *auto*, always')
            [CompletionResult]::new('--color', 'color', [CompletionResultType]::ParameterName, 'When to use colors: never, *auto*, always')
            [CompletionResult]::new('-j', 'j', [CompletionResultType]::ParameterName, 'j')
            [CompletionResult]::new('--threads', 'threads', [CompletionResultType]::ParameterName, 'threads')
            [CompletionResult]::new('-S', 'S', [CompletionResultType]::ParameterName, 'Limit results based on the size of files.')
            [CompletionResult]::new('--size', 'size', [CompletionResultType]::ParameterName, 'Limit results based on the size of files.')
            [CompletionResult]::new('--max-buffer-time', 'max-buffer-time', [CompletionResultType]::ParameterName, 'max-buffer-time')
            [CompletionResult]::new('--changed-within', 'changed-within', [CompletionResultType]::ParameterName, 'Filter by file modification time (newer than)')
            [CompletionResult]::new('--changed-before', 'changed-before', [CompletionResultType]::ParameterName, 'Filter by file modification time (older than)')
            [CompletionResult]::new('--max-results', 'max-results', [CompletionResultType]::ParameterName, 'max-results')
            [CompletionResult]::new('--base-directory', 'base-directory', [CompletionResultType]::ParameterName, 'base-directory')
            [CompletionResult]::new('--path-separator', 'path-separator', [CompletionResultType]::ParameterName, 'path-separator')
            [CompletionResult]::new('--search-path', 'search-path', [CompletionResultType]::ParameterName, 'search-path')
            [CompletionResult]::new('-H', 'H', [CompletionResultType]::ParameterName, 'Search hidden files and directories')
            [CompletionResult]::new('--hidden', 'hidden', [CompletionResultType]::ParameterName, 'Search hidden files and directories')
            [CompletionResult]::new('-I', 'I', [CompletionResultType]::ParameterName, 'Do not respect .(git|fd)ignore files')
            [CompletionResult]::new('--no-ignore', 'no-ignore', [CompletionResultType]::ParameterName, 'Do not respect .(git|fd)ignore files')
            [CompletionResult]::new('--no-ignore-vcs', 'no-ignore-vcs', [CompletionResultType]::ParameterName, 'no-ignore-vcs')
            [CompletionResult]::new('--no-global-ignore-file', 'no-global-ignore-file', [CompletionResultType]::ParameterName, 'no-global-ignore-file')
            [CompletionResult]::new('-u', 'u', [CompletionResultType]::ParameterName, 'u')
            [CompletionResult]::new('--unrestricted', 'unrestricted', [CompletionResultType]::ParameterName, 'unrestricted')
            [CompletionResult]::new('-s', 's', [CompletionResultType]::ParameterName, 'Case-sensitive search (default: smart case)')
            [CompletionResult]::new('--case-sensitive', 'case-sensitive', [CompletionResultType]::ParameterName, 'Case-sensitive search (default: smart case)')
            [CompletionResult]::new('-i', 'i', [CompletionResultType]::ParameterName, 'Case-insensitive search (default: smart case)')
            [CompletionResult]::new('--ignore-case', 'ignore-case', [CompletionResultType]::ParameterName, 'Case-insensitive search (default: smart case)')
            [CompletionResult]::new('-g', 'g', [CompletionResultType]::ParameterName, 'Glob-based search (default: regular expression)')
            [CompletionResult]::new('--glob', 'glob', [CompletionResultType]::ParameterName, 'Glob-based search (default: regular expression)')
            [CompletionResult]::new('--regex', 'regex', [CompletionResultType]::ParameterName, 'regex')
            [CompletionResult]::new('-F', 'F', [CompletionResultType]::ParameterName, 'F')
            [CompletionResult]::new('--fixed-strings', 'fixed-strings', [CompletionResultType]::ParameterName, 'fixed-strings')
            [CompletionResult]::new('-a', 'a', [CompletionResultType]::ParameterName, 'Show absolute instead of relative paths')
            [CompletionResult]::new('--absolute-path', 'absolute-path', [CompletionResultType]::ParameterName, 'Show absolute instead of relative paths')
            [CompletionResult]::new('-l', 'l', [CompletionResultType]::ParameterName, 'Use a long listing format with file metadata')
            [CompletionResult]::new('--list-details', 'list-details', [CompletionResultType]::ParameterName, 'Use a long listing format with file metadata')
            [CompletionResult]::new('-L', 'L', [CompletionResultType]::ParameterName, 'Follow symbolic links')
            [CompletionResult]::new('--follow', 'follow', [CompletionResultType]::ParameterName, 'Follow symbolic links')
            [CompletionResult]::new('-p', 'p', [CompletionResultType]::ParameterName, 'Search full path (default: file-/dirname only)')
            [CompletionResult]::new('--full-path', 'full-path', [CompletionResultType]::ParameterName, 'Search full path (default: file-/dirname only)')
            [CompletionResult]::new('-0', '0', [CompletionResultType]::ParameterName, 'Separate results by the null character')
            [CompletionResult]::new('--print0', 'print0', [CompletionResultType]::ParameterName, 'Separate results by the null character')
            [CompletionResult]::new('--prune', 'prune', [CompletionResultType]::ParameterName, 'prune')
            [CompletionResult]::new('-1', '1', [CompletionResultType]::ParameterName, '1')
            [CompletionResult]::new('--show-errors', 'show-errors', [CompletionResultType]::ParameterName, 'show-errors')
            [CompletionResult]::new('--one-file-system', 'one-file-system', [CompletionResultType]::ParameterName, 'one-file-system')
            [CompletionResult]::new('-h', 'h', [CompletionResultType]::ParameterName, 'Prints help information')
            [CompletionResult]::new('--help', 'help', [CompletionResultType]::ParameterName, 'Prints help information')
            [CompletionResult]::new('-V', 'V', [CompletionResultType]::ParameterName, 'Prints version information')
            [CompletionResult]::new('--version', 'version', [CompletionResultType]::ParameterName, 'Prints version information')
            break
        }
    })

    $completions.Where{ $_.CompletionText -like "$wordToComplete*" } |
        Sort-Object -Property ListItemText
}
