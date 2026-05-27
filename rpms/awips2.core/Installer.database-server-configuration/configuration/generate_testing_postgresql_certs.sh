#!/bin/bash

# Makes a new certificate chain for use with AWIPS PostgreSQL, including root,
# server, and client certificates and key files. Output files are placed in a
# temporary directory, whose path will be provided in the output of the script
# when it is finished.
#
# The output files include root.key, which should not be deployed, but may be
# used to issue additional certificates.
#
# The output files are to be used for testing purposes ONLY!
#
# Note: I chose the expiration date of six years from now to land in 2029,
# based on NIST's estimation that RSA-2048 will be secure until about 2030.
#
# Author: tgurney 2023/05/10

origdir="$(dirname "$0")"
tmpdir="$(mktemp -d)"
cleanup_exit() {
    cd "${origdir}"
    rm -rf "${tmpdir}"
    exit $1
}
die() {
    cleanup_exit 1
}
trap cleanup_exit SIGINT SIGTERM

cd "${tmpdir}" || die
mkdir -p newcerts || die
touch index.txt || die

cat > v1.cnf << 'EOF'
HOME = .
openssl_conf = default_modules

[ default_modules ]
ssl_conf = ssl_module

[ ssl_module ]
system_default = crypto_policy

[ crypto_policy ]
.include /etc/crypto-policies/back-ends/opensslcnf.config

[ ca ]
default_ca = CA_default

[ CA_default ]
dir = .
certs = $dir/certs
crl_dir = $dir/crl
database = $dir/index.txt
unique_subject = no
new_certs_dir = $dir/newcerts
certificate = $dir/root.crt
serial = $dir/serial
crlnumber = $dir/crlnumber
private_key = $dir/root.key
policy = policy_match
name_opt = ca_default
cert_opt = ca_default
default_days = 2190
default_md = sha256
preserve = no

[ policy_match ]
countryName = match
stateOrProvinceName = match
localityName = match
organizationName = match
organizationalUnitName = optional
commonName = supplied
emailAddress = optional

[ req ]
default_bits = 2048
default_md = sha256
default_keyfile = privkey.pem
distinguished_name = req_distinguished_name
string_mask = utf8only

[ req_distinguished_name ]
countryName = Country Name (2 letter code)
countryName_default = US
countryName_min = 2
countryName_max = 2
stateOrProvinceName = State or Province Name (full name)
stateOrProvinceName_default = Nebraska
localityName = Locality Name (eg, city)
localityName_default = Omaha
0.organizationName = Organization Name (eg, company)
0.organizationName_default = Raytheon
organizationalUnitName = Organizational Unit Name (eg, section)
organizationalUnitName_default = TESTING AWIPS
commonName = Common Name (eg, your name or your server\'s hostname)
commonName_max = 64
emailAddress = Email Address
emailAddress_max = 64
EOF

echo "*** Generating root.crt and root.key:"
openssl req -x509 -new -keyout root.key -days 2190 -out root.crt -sha256 -nodes -subj "/C=US/ST=Nebraska/L=Omaha/O=Raytheon TESTING/OU=AWIPS TESTING/CN=TESTING PostgreSQL Root CA" || die
echo "*** Done generating root.crt and root.key."

for name in localhost awips awipsadmin pguser postgres replication; do
    echo "*** Generating $name.crt and $name.key:"
    openssl req -new -sha256 -nodes -out $name.req -subj "/C=US/ST=Nebraska/L=Omaha/O=Raytheon TESTING/OU=AWIPS TESTING/CN=$name" -keyout $name.key -config v1.cnf || die
    yes | openssl ca -in $name.req -rand_serial -config v1.cnf -out $name.crt || die
    echo "*** Done generating $name.crt and $name.key."
    if [[ "$name" != "localhost" ]]; then
        openssl pkcs8 -topk8 -inform PEM -outform DER -in $name.key -out $name.pk8 -nocrypt || die
    fi
done

mv localhost.crt server.crt
mv localhost.key server.key
rm -rf *.req index.txt index.txt.* newcerts serial.* serial v1.cnf

echo
echo "*** Output files are located at $tmpdir"
exit 0
