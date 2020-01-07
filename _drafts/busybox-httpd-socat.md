## On the server

# AWS EC2
SERVER_IP=$(curl http://169.254.169.254/latest/meta-data/public-ipv4)

# Digital Ocean
SERVER_IP=$(curl http://169.254.169.254/metadata/v1/interfaces/public/0/ipv4/address)

# Generate a self-signed server certificate
openssl genrsa -out server.key 4096
openssl rsa -in server.key -out server.key
openssl req -sha256 -new -key server.key -out server.csr -subj "/CN=$SERVER_IP"
openssl x509 -req -sha256 -days 7 -in server.csr -signkey server.key -out server.crt
rm server.csr
cat server.crt server.key > server.pem

# Display the certificate fingerprint
openssl x509 -in server.crt -fingerprint -noout

# Publish your files
mkdir public_html
echo "Hello World" > public_html/hello
cp server.crt public_html

# Create a password
tee httpd.conf <<EOF
/:$USER:$(env LC_CTYPE=C tr -dc 'A-Za-z0-9_-' < /dev/urandom | head -c 16)
EOF

# Run the server
busybox httpd -f -c $(pwd)/httpd.conf -p 127.0.0.1:15301 -h $(pwd)/public_html &

# TLS tunnel
socat openssl-listen:15300,reuseaddr,cert=server.pem,verify=0,fork tcp:127.0.0.1:15301 &

# Open the firewall
sudo ufw allow 15300/tcp

## On the client

SERVER_IP=1.2.3.4   # or whatever

# Download the certificate
curl -u 'user:pass' --insecure https://$SERVER_IP:15300/server.crt -o server.crt

# Verify the certificate fingerprint
openssl x509 -in server.crt -fingerprint -noout

# Use the certificate
curl -u 'user:pass' --cacert server.crt https://$SERVER_IP:15300/hello

## Clean up the server
sudo ufw delete allow 15300/tcp
fg # then Ctrl+C
fg # then Ctrl+C
rm httpd.conf
rm -rf public_html
rm server.crt
rm server.key
rm server.pem
