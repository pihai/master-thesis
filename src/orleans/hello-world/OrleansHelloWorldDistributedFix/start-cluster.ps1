cd GrainsCollection\bin\Debug

Write-Host "Starting primary silo ..."
Start-Process "OrleansHost.exe" "Primary"

Write-Host "Waiting for primary to boot up ..."
Start-Sleep -s 15

Write-Host "Starting secondary nodes ..."
Start-Process "OrleansHost.exe" "Secondary1"
Start-Sleep -s 1
Start-Process "OrleansHost.exe" "Secondary2"
Start-Sleep -s 2
Start-Process "OrleansHost.exe" "Secondary3"
Start-Sleep -s 3
Start-Process "OrleansHost.exe" "Secondary4"