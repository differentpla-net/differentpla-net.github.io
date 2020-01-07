# OpenVPN

## References

- https://www.digitalocean.com/community/tutorials/how-to-set-up-an-openvpn-server-on-ubuntu-18-04
- https://www.digitalocean.com/community/tutorials/initial-server-setup-with-ubuntu-18-04

## Server

- Digital Ocean Droplet, smallest available, London.

ssh root@104.248.166.104
root@openvpn-lon1-01:~# adduser roger
Adding user `roger' ...
Adding new group `roger' (1000) ...
Adding new user `roger' (1000) with group `roger' ...
Creating home directory `/home/roger' ...
Copying files from `/etc/skel' ...
Enter new UNIX password:
Retype new UNIX password:
passwd: password updated successfully
Changing the user information for roger
Enter the new value, or press ENTER for the default
	Full Name []: Roger Lipscombe
	Room Number []:
	Work Phone []:
	Home Phone []:
	Other []:
Is the information correct? [Y/n] y
root@openvpn-lon1-01:~# usermod -aG sudo roger
root@openvpn-lon1-01:~# echo "%sudo ALL = (ALL) NOPASSWD: ALL" >> /etc/sudoers.d/sudo


root@openvpn-lon1-01:~# cp -r .ssh/ /home/roger/.ssh/
root@openvpn-lon1-01:/home/roger# chown -R roger.roger /home/roger/.ssh


root@openvpn-lon1-01:~# ufw app list
Available applications:
  OpenSSH
root@openvpn-lon1-01:~# ufw status
Status: inactive
root@openvpn-lon1-01:~# ufw allow OpenSSH
Rules updated
Rules updated (v6)
root@openvpn-lon1-01:~# ufw status
Status: inactive
root@openvpn-lon1-01:~# ufw enable
Command may disrupt existing ssh connections. Proceed with operation (y|n)? y
Firewall is active and enabled on system startup
root@openvpn-lon1-01:~# ufw status
Status: active

To                         Action      From
--                         ------      ----
OpenSSH                    ALLOW       Anywhere
OpenSSH (v6)               ALLOW       Anywhere (v6)

ssh roger@104.248.166.104
sudo bash # OK


# as roger
sudo apt update
sudo apt upgrade



wget -P ~/ https://github.com/OpenVPN/easy-rsa/releases/download/v3.0.4/EasyRSA-3.0.4.tgz

roger@openvpn-lon1-01:~$ tar xf EasyRSA-3.0.4.tgz
roger@openvpn-lon1-01:~$ chmod 700 EasyRSA-3.0.4
roger@openvpn-lon1-01:~$ cd EasyRSA-3.0.4/


roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ vim vars
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ ./easyrsa init-pki

Note: using Easy-RSA configuration from: ./vars

init-pki complete; you may now create a CA or requests.
Your newly created PKI dir is: /home/roger/EasyRSA-3.0.4/pki

roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ ./easyrsa build-ca nopass

Note: using Easy-RSA configuration from: ./vars
Generating a 2048 bit RSA private key
.............................................................+++
...................................................+++
writing new private key to '/home/roger/EasyRSA-3.0.4/pki/private/ca.key.goOQ1c3oDk'


roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ ./easyrsa gen-req server nopass

Note: using Easy-RSA configuration from: ./vars
Generating a 2048 bit RSA private key
..+++
......................................................................+++
writing new private key to '/home/roger/EasyRSA-3.0.4/pki/private/server.key.muaa38A6EV'
-----
You are about to be asked to enter information that will be incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Common Name (eg: your user, host, or server name) [server]:

Keypair and certificate request completed. Your files are:
req: /home/roger/EasyRSA-3.0.4/pki/reqs/server.req
key: /home/roger/EasyRSA-3.0.4/pki/private/server.key

**Note that I'm running the CA and server PKI on the same host; #yolo**


The SSL country code for the UK is GB.


roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ ./easyrsa sign-req server server

Note: using Easy-RSA configuration from: ./vars


You are about to sign the following certificate.
Please check over the details shown below for accuracy. Note that this request
has not been cryptographically verified. Please be sure it came from a trusted
source or that you have verified the request checksum with the sender.

Request subject, to be signed as a server certificate for 3650 days:

subject=
    commonName                = server


Type the word 'yes' to continue, or any other input to abort.
  Confirm request details: yes
Using configuration from ./openssl-easyrsa.cnf
Can't open /home/roger/EasyRSA-3.0.4/pki/index.txt.attr for reading, No such file or directory
140015450567104:error:02001002:system library:fopen:No such file or directory:../crypto/bio/bss_file.c:74:fopen('/home/roger/EasyRSA-3.0.4/pki/index.txt.attr','r')
140015450567104:error:2006D080:BIO routines:BIO_new_file:no such file:../crypto/bio/bss_file.c:81:
Check that the request matches the signature
Signature ok
The Subject's Distinguished Name is as follows
commonName            :ASN.1 12:'server'
Certificate is to be certified until Dec  2 15:42:52 2028 GMT (3650 days)

Write out database with 1 new entries
Data Base Updated

Certificate created at: /home/roger/EasyRSA-3.0.4/pki/issued/server.crt

roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ sudo cp pki/ca.crt /etc/openvpn/
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ sudo cp pki/
.rnd               ca.crt.GOhBaycTzJ  index.txt          index.txt.old      private/           serial
ca.crt             certs_by_serial/   index.txt.attr     issued/            reqs/              serial.old
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ sudo cp pki/issued/server.crt /etc/openvpn/


./easyrsa gen-dh

roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ openvpn --genkey --secret ta.key
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ sudo cp ta.key /etc/openvpn/
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ sudo cp pki/dh.pem /etc/openvpn/
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ cd
roger@openvpn-lon1-01:~$ mkdir -p ~/openvpn-clients/keys
roger@openvpn-lon1-01:~$ chmod -R 700 openvpn-clients/
roger@openvpn-lon1-01:~$ cd EasyRSA-3.0.4/
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ ./easyrsa gen-req client1 nopass

Note: using Easy-RSA configuration from: ./vars
Generating a 2048 bit RSA private key
................+++
.........................+++
writing new private key to '/home/roger/EasyRSA-3.0.4/pki/private/client1.key.RCXhJq2gv3'
-----
You are about to be asked to enter information that will be incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Common Name (eg: your user, host, or server name) [client1]:

Keypair and certificate request completed. Your files are:
req: /home/roger/EasyRSA-3.0.4/pki/reqs/client1.req
key: /home/roger/EasyRSA-3.0.4/pki/private/client1.key

roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ cp pki/private/client1.key ~/openvpn-clients/keys/
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ ./easyrsa sign-req client client1

Note: using Easy-RSA configuration from: ./vars


You are about to sign the following certificate.
Please check over the details shown below for accuracy. Note that this request
has not been cryptographically verified. Please be sure it came from a trusted
source or that you have verified the request checksum with the sender.

Request subject, to be signed as a client certificate for 3650 days:

subject=
    commonName                = client1


Type the word 'yes' to continue, or any other input to abort.
  Confirm request details:

Aborting without confirmation.
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ ./easyrsa sign-req client client1

Note: using Easy-RSA configuration from: ./vars


You are about to sign the following certificate.
Please check over the details shown below for accuracy. Note that this request
has not been cryptographically verified. Please be sure it came from a trusted
source or that you have verified the request checksum with the sender.

Request subject, to be signed as a client certificate for 3650 days:

subject=
    commonName                = client1


Type the word 'yes' to continue, or any other input to abort.
  Confirm request details: yes
Using configuration from ./openssl-easyrsa.cnf
Check that the request matches the signature
Signature ok
The Subject's Distinguished Name is as follows
commonName            :ASN.1 12:'client1'
Certificate is to be certified until Dec  2 15:54:52 2028 GMT (3650 days)

Write out database with 1 new entries
Data Base Updated

Certificate created at: /home/roger/EasyRSA-3.0.4/pki/issued/client1.crt

roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ cp pki/issued/client1.crt ~/openvpn-clients/keys/
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ cp /etc/openvpn/ca.crt ~/openvpn-clients/keys/
cp: cannot open '/etc/openvpn/ca.crt' for reading: Permission denied
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ sudo cp /etc/openvpn/ca.crt ~/openvpn-clients/keys/
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ sudo cp /usr/share/doc/
Display all 532 possibilities? (y or n)
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ sudo cp /usr/share/doc/openvpn/examples/sample-config-files/server.conf.gz /etc/openvpn/server.conf.gz
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ sudo gzip -d /etc/openvpn/server.conf.gz
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ sudo vim /etc/openvpn/server
server/      server.conf  server.crt   server.key
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$ sudo vim /etc/openvpn/server.conf
roger@openvpn-lon1-01:~/EasyRSA-3.0.4$


roger@openvpn-lon1-01:~$ vim ~/client-configs/base.conf
roger@openvpn-lon1-01:~$ vim ~/client-configs/make_config.sh
roger@openvpn-lon1-01:~$ chmod 700 ~/client-configs/make_config.sh
roger@openvpn-lon1-01:~$ ls -l
total 52
drwx------ 5 roger roger  4096 Dec  5 15:52 EasyRSA-3.0.4
-rw-rw-r-- 1 roger roger 37721 Jan 21  2018 EasyRSA-3.0.4.tgz
drwx------ 4 roger roger  4096 Dec  5 16:21 client-configs
roger@openvpn-lon1-01:~$ cd client-configs/
roger@openvpn-lon1-01:~/client-configs$ ls
base.conf  files  keys  make_config.sh
roger@openvpn-lon1-01:~/client-configs$ ./make_config.sh client1
cat: /home/roger/client-configs/keys/ca.crt: Permission denied
cat: /home/roger/client-configs/keys/ta.key: No such file or directory
roger@openvpn-lon1-01:~/client-configs$ ls -l
total 16
-rw-r--r-- 1 roger roger 3691 Dec  5 16:21 base.conf
drwxrwxr-x 2 roger roger 4096 Dec  5 16:22 files
drwx------ 2 roger roger 4096 Dec  5 15:55 keys
-rwx------ 1 roger roger  465 Dec  5 16:21 make_config.sh
roger@openvpn-lon1-01:~/client-configs$ ls -l keys/
total 16
-rw------- 1 root  root  1172 Dec  5 15:55 ca.crt
-rw------- 1 roger roger 4437 Dec  5 15:55 client1.crt
-rw------- 1 roger roger 1700 Dec  5 15:54 client1.key
roger@openvpn-lon1-01:~/client-configs$ chown roger.roger keys/ca.crt
chown: changing ownership of 'keys/ca.crt': Operation not permitted
roger@openvpn-lon1-01:~/client-configs$ sudo chown roger.roger keys/ca.crt
roger@openvpn-lon1-01:~/client-configs$


sudo ufw allow 1194/udp
sudo ufw reload

DigitalOcean page gives a script for creating a .ovpn file from the keys, etc.

Transfer this to your Android device. I used scp to my desktop PC, then AirDroid. You could use scp to a desktop PC, and then transfer using a microSD card.

You could use termux and openssh:

$ termux-setup-storage
$ apt update && apt upgrade
$ pkg install openssh
$ ssh-keygen
$ cp .ssh/id_rsa.pub ~/storage/downloads/

Then, from there, copy the public key to the server's authorized_keys. This probably means going through the same hoops in the other direction. In my case, getting from termux to my desktop PC: Slack, upload file (it's a public key, so no big deal), it's a Samsung tablet, so choose "Documents" > "Termux", and select the file from there.

From there, copy it to `.ssh/authorized_keys` on the relevant server. Since I already had the .openvpn file on my desktop PC, I stopped at that point.



