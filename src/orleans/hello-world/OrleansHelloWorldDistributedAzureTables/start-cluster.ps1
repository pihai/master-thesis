cd GrainsCollection\bin\Debug

Write-Host "Starting silos ..."
Start-Process "OrleansHost.exe" "Node0"
Start-Process "OrleansHost.exe" "Node1"
Start-Process "OrleansHost.exe" "Node2"
Start-Process "OrleansHost.exe" "Node3"
Start-Process "OrleansHost.exe" "Node4"