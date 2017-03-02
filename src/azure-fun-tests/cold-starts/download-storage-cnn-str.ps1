Try {
  Get-AzureRmContext
} Catch {
  if ($_ -like "*Login-AzureRmAccount to login*") {
    Login-AzureRmAccount
  }
}

$storageResGrpName = "Garbage"
$storageName = "hptimingstorage"
$storageKeys = Get-AzureRmStorageAccountKey -ResourceGroupName $storageResGrpName -AccountName $storageName
$storageKey = $storageKeys.Item(0).Value
$storageCnnStr = "DefaultEndpointsProtocol=https;AccountName=$storageName;AccountKey=$storageKey"
New-Item "storagekey.secrets" -ItemType File -Value $storageCnnStr