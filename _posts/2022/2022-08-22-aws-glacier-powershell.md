---
title: "AWS Glacier with PowerShell"
date: 2022-08-22T08:10:00Z
tags: aws aws-glacier powershell
---

I needed to delete a Glacier vault, which (because it's not empty) needs the use of an SDK or the CLI. Since I'm using
Windows, I figured that I might as well take the opportunity to play with the AWS Tools for PowerShell.

## Background

I was looking at the bill for my personal AWS account (a whole $8/mo...), and I noticed that about $2 of that was for
Amazon Glacier, which I haven't used since experimenting with it a while ago. The AWS web interface won't delete a vault
if it contains any data, so you need to use the REST API, one of the SDKs, or the AWS CLI. So I used the AWS Tools for
PowerShell.

## Installation

The AWS Tools for PowerShell are published to the PowerShell gallery. They're broken into separate modules, so you need
to install them individually. For example:

```powershell
Install-Module -Name AWS.Tools.Glacier
```

AWS provides the `AWS.Tools.Installer` module, but I don't really see the point, since you use that like this:

```powershell
Install-Module -Name AWS.Tools.Installer
Install-AWSToolsModule Glacier
```

...which is _more_ typing. The only advantage seems to be that it accepts a comma-separated list of modules to install:

```powershell
Install-AWSToolsModule EC2,S3,Glacier   # etc.
```

## Listing exported commands

An aside:

```powershell
PS> Get-Module AWS.Tools.Glacier | % { $_.ExportedCommands.Keys } | sort
Add-GLCTagsToVault
Complete-GLCVaultLock
Get-GLCDataRetrievalPolicy
...
```

## Credentials

Initially, any attempt to use a command fails with something like the following:

```powershell
PS> Get-GLCVaultList
Get-GLCVaultList: No credentials specified or obtained from persisted/shell defaults.
```

If you're also using the original AWS CLI, which expects credentials in `~/.aws/credentials`, the PowerShell tools will
use those credentials. Otherwise, you can use the `Set-AWSCredential` command:

```powershell
Set-AWSCredential -AccessKey AKIA.... -SecretKey ... -StoreAs MyProfileName
Initialize-AWSDefaultConfiguration -ProfileName MyProfileName -Region eu-west-2     # eu-west-2 is London
```

## Listing Vaults

```powershell
PS> Get-GLCVaultList

CreationDate      : 07/08/2020 09:29:02
LastInventoryDate : 14/08/2020 16:46:08
NumberOfArchives  : 125926
SizeInBytes       : 622319054237
VaultARN          : arn:aws:glacier:eu-west-2:MyAccount:vaults/MyVault
VaultName         : MyVault
```

As you can see, I have a single vault (I deleted the empty ones in the web interface), which I don't seem to have
touched since 2020.

## Deleting archives

```powershell
PS> Remove-GLCVault -Force -VaultName MyVault
Remove-GLCVault: Vault not empty or recently written to: arn:aws:glacier:eu-west-2:<account>:vaults/MyVault
```

Nope. Still can't delete the vault; I guess we really _do_ have to delete the archives first.

## Getting an inventory

To get the list of archives in the vault, we need to start an asynchronous "inventory-retrieval" job:

```powershell
PS> Start-GLCJob -VaultName MyVault -JobType inventory-retrieval
# ...outputs JobId, JobOutputPath, Location fields.
```

Then we just periodically query the job to see if it's finished:

```powershell
PS> GetGLCJobList -VaultName MyVault
```

I don't know how long the job is going to take to run. It feels like checking every hour or so would be OK.

Now we wait.

<div class="callout callout-success" markdown="span">
I went to the gym and then had lunch. After about 4 hours, the job was complete.
</div>

The documentation says that the results will be kept for at least 24 hours.

### Getting the job output

```powershell
Read-GLCJobOutput -VaultName MyVault -JobId The-Job-Id -FilePath C:\Users\roger\job.json
```

### Parsing the job output

It's JSON, so we can use `ConvertFrom-JSON` to turn it into a variable:

```powershell
$job = Get-Content C:\Users\roger\job.json | ConvertFrom-JSON
```

## Deleting the archives

```powershell
$job.ArchiveList | % { Remove-GLCArchive -VaultName MyVault -ArchiveId $_.ArchiveId -Force }
```

...or with progress:

```powershell
$curr = 0
$count = $job.ArchiveList.Count
foreach ($it in $job.ArchiveList) {
    $curr++
    $pct = (100 * $curr / $count) + 1
    if ($pct > 100) { $pct = 100 }
    Write-Progress -Activity "Removing Archives" -Status "${curr} / ${count}" -PercentComplete $pct
    Remove-GLCArchive -VaultName MyVault -ArchiveId $it.ArchiveId -Force
}
```

There are more accurate ways to calculate the exact percentage, but this'll do. Note that `-PercentComplete 0` shows
100%, so we add 1 to the calculated value. Because we've done that, we also need to make sure we don't go over 100%.

To delete roughly 126,000 items, this took just under an hour and a half.

## Deleting the vault?

So we should be good to delete the vault, then?

```powershell
PS> Remove-GLCVault -Force -VaultName MyVault
Remove-GLCVault: Vault not empty or recently written to: arn:aws:glacier:eu-west-2:<account>:vaults/MyVault
```

No.

I suspect that either I've screwed up something in the `Remove-GLCArchive` loop above, or that AWS Glacier won't notice
that the vault's empty until it generates a new inventory. According to the documentation, that happens approximately
once a day. Either way, I won't know until that happens.

So I guess I'll just leave it until tomorrow morning, and then I'll check the inventory status every few hours with the
following command:

```powershell
Get-GLCVault -VaultName MyVault | Select-Object LastInventoryDate
```

## Deleting the vault

It's 9am the following morning, and the inventory's been updated...

```powershell
PS> (Get-GLCVault -VaultName MyVault).LastInventoryDate

Tue 23 August 2022 00:28:14
```

...and the vault can be deleted:

```powershell
PS> Remove-GLCVault -Force -VaultName MyVault
# no output
PS> Get-GLCVaultList
# no output
```

## References

- <https://docs.aws.amazon.com/powershell/latest/userguide/specifying-your-aws-credentials.html>
- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html>
