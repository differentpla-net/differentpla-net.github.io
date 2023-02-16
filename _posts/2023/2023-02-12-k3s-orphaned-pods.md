Feb 12 17:31:06 roger-nuc2 k3s[822]: E0212 17:31:06.285362     822 kubelet_volumes.go:245] "There were many similar errors. Turn up verbosity to see them." err="orphaned pod \"00ea640c-41da-4374-b6e8-7ed091559478\" found, but error occurred when trying to remove the volumes dir: not a directory" numErrs=3



```
sudo -i
ls /var/lib/kubelet/pods/*/volumes/kubernetes.io~csi/pvc*
```

Related:

https://ma.ttias.be/what-exactly-being-sent-ubuntu-motd/

```
cat /var/lib/update-notifier/updates-available
cat /var/run/reboot-required
```

https://zerokspot.com/weblog/2021/02/08/need-a-reboot/

Doesn't apparently work on minimized Ubuntu.
