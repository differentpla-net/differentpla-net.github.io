---
title: "PowerShell Closures"
date: 2012-05-25T12:07:04.000Z
---

```powershell
# Create a script block. It doesn't capture $x.
$x = 'A'
$sb = { Write-Host $x }

# Change the value of $x and run the script block. Note that it displays the current value.
$x = 'B'
& $sb                  # ... B

# Create a closure.
$closure = $sb.GetNewClosure()

# Change the value of $x.
$x = 'C'

# Run the closure. It displays the original value. Success.
& $closure             # ... B

# For comparison, run the script block. Nope. Still tracking the current value.
& $sb                  # ... C
```
