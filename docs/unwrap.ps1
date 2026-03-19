$filepath = "c:\Degi\GitHub\wanulcas\docs\w_notes.md"
$content = Get-Content -Path $filepath -Raw -Encoding UTF8

# Handle \n or \r\n properly. 
$content = $content -replace "`r`n", "`n"
$blocks = $content -split "`n`n"
$newBlocks = @()

foreach ($block in $blocks) {
    if ($block.StartsWith("```")) {
        $newBlocks += $block
        continue
    }
    
    $trimmed = $block.TrimStart()
    if ($trimmed.StartsWith("#") -or $trimmed.StartsWith("|") -or $trimmed.StartsWith(">") -or $trimmed.StartsWith("-") -or $trimmed.StartsWith("*") -or $trimmed.StartsWith("!") -or $trimmed.StartsWith("Table ") -or ($trimmed -match "Figure \d+")) {
        $newBlocks += $block
        continue
    }

    $lines = $block -split "`n"
    if ($lines.Length -eq 1) {
        $newBlocks += $block
        continue
    }

    $joined = $lines[0]
    for ($i = 1; $i -lt $lines.Length; $i++) {
        $line = $lines[$i]
        if ($joined.EndsWith(" ")) {
            $joined += $line.TrimStart()
        } else {
            $joined += " " + $line.TrimStart()
        }
    }
    $newBlocks += $joined
}

$newContent = $newBlocks -join "`r`n`r`n"
Set-Content -Path $filepath -Value $newContent -Encoding UTF8
Write-Output "Unwrapped paragraphs."
