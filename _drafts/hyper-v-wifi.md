# Connecting a Hyper-V VM to the Internet over Wifi, on Windows 10 (1903, 18922.1000)

https://docs.microsoft.com/en-us/virtualization/hyper-v-on-windows/user-guide/setup-nat-network

Hyper-V Quick Create: Ubuntu 18.04.2 LTS

Initially, the network card is connected to the default switch.

Create a new virtual switch, type Internal. Call it "NAT Gateway".

In an Administrator PowerShell:

    Get-NetAdapter | ? { $_.Name -like '*NAT Gateway*' } | select ifIndex

