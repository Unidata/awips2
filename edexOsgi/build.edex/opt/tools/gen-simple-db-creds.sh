output_dir=output
db_users=(awips awipsadmin pguser postgres)
unpriv_db_users=(awips pguser)
dn_attrs='/C=US/ST=Maryland/L=Silver Spring/O=Raytheon/OU=AWIPS'
validity_days=$((30 * 365))

site=${SITE_IDENTIFIER}
if test -n "$site"; then
    ca_cn=dx1-$site
    # Clients connect with "dx1f" and not the full host name
    db_server_host=dx1f
fi

usage() {
    echo "$(basename "$0") [options]"
    echo "  Generates self-signed certificates for testing PostgreSQL certificate-based authentication."
    echo ""
    echo "  Files are written to {current directory}/output:"
    echo "     root.{crt,key,srl} - root cert, key, and serial file"
    echo "     server.{crt,key}   - database cert and key"
    echo "     {db user}.{crt,key,pk8} - database user crt, key, and DER-formatted key"
    echo "     server.tgz  - bundle for database server (Extract in /awips2/data on dx1f.)"
    echo "     awips.tgz   - bundle for admin users with .postgresql/ prefix (Extract in /home/awips/ .)"
    echo "     user.tgz    - bundle for app users with .postgresql/ prefix (Extract in /home/{user}/ .)"
    echo "     alldb.tgz   - contains all db user (including admin) certs/keys with no directory prefix"
    echo ""
    echo "  Options:"
    echo "   -c {name}   Specify the root certificate CN [dx1-\$SITE_IDENTIFIER]"
    echo "   -d {name}   Specify the database server host name [dx1f-\$SITE_IDENTIFIER]"
    echo "   -D {attrs}  Distinguished name attributes"
    echo "   -h          Display usage"
    echo "   -s          Use current host name for CA and database server CNs"
}

while test "$#" -gt 0; do
    case "$1" in
        -c) ca_cn=$2 ; shift ;;
        -d) db_server_host=$2 ; shift ;;
        -D) dn_attrs=$2 ; shift ;;
        -s)
            ca_cn=$(hostname)
            db_server_host=$ca_cn
            ;;
        -h) usage ; exit 0 ;;
	*) usage ; exit 1 ;;
    esac
    shift
done

if [[ -z $ca_cn || -z $db_server_host ]]; then
    echo "error: CA and database host CNs not defined. (\$SITE_IDENTIFIER not set?  Maybe use -s option?)" >&2
    exit 1
fi

# # For testing
# set -x

gen_self_signed() {
    local name=$1
    local subj=$2
    test -n "$name" && test -n "$subj" || { echo "self_sign: need base name and subject" >&2 ; return 1 ; }
    : \
        && openssl req -new -subj "$subj" -out "$name".req -nodes -keyout "$name".key \
        && openssl req -x509 -days "$validity_days" -in "$name".req -key "$name".key -out "$name".crt \
        && chmod g=,o= "$name".key \
        || return 1
}

gen_cert() {
    local name=$1 subj=$2 ca=$3
    test -n "$name" && test -n "$subj" && test -n "$ca" || { echo "cert: need base name, subject, and CA" >&2 ; return 1 ; }
    local serial_fn=${ca}.srl
    local serial_opt=""
    if test ! -e "$serial_fn"; then
        serial_opt=-CAcreateserial
    fi
    : \
        && openssl req -new -subj "$subj" -newkey rsa:2048 -nodes -keyout "$name".key -out "$name".req \
        && openssl x509 -days "$validity_days" -req -in "$name".req \
            -CA "$ca".crt -CAkey "$ca".key $serial_opt -out "$name".crt \
        && openssl pkcs8 -nocrypt -in "$name".key -topk8 -outform der -out "$name".pk8 \
        && chmod g=,o= "$name".key "$name".pk8 \
        || return 1
}

awips_og=(--owner=awips --group=fxalpha)
gen_dot_postgresql() {
    local dest=$1
    shift
    rm -rf gtc0
    local dotpg=gtc0/.postgresql
    mkdir -p "$dotpg" \
        && chmod a=,u=rwx "$dotpg" \
        && cp -p "$@" "$dotpg" \
        && ln -s awips.crt "$dotpg"/postgresql.crt \
        && ln -s awips.key "$dotpg"/postgresql.key \
        && ln -s awips.pk8 "$dotpg"/postgresql.pk8 \
        && tar czf "$dest" -C gtc0 "${awips_og[@]}" .postgresql \
        && rm -rf gtc0 \
        && return 0
}

cred_files() {
    local output=''
    for name in "$@"; do
        output="$output "$(echo $name.{crt,key,pk8})
    done
    echo "$output"
}

dn() {
    echo "$dn_attrs/CN=$1"
}

if test -e "$output_dir"; then
    echo "$output_dir: already exists" >&2
    exit 1
fi
mkdir -p "$output_dir" \
    && chmod a=,u=rwx "$output_dir" \
    && cd "$output_dir" || exit 1

gen_self_signed root "$(dn "$ca_cn")" || exit 1
gen_cert server "$(dn "$db_server_host")" root || exit 1
for user in "${db_users[@]}"; do
    gen_cert "$user" "$(dn "$user")" root || exit 1
done

tar czf server.tgz "${awips_og[@]}"       root.crt server.{crt,key} \
    && tar czf alldb.tgz "${awips_og[@]}" root.crt $(cred_files "${db_users[@]}") \
    && gen_dot_postgresql awips.tgz       root.crt $(cred_files "${db_users[@]}") \
    && gen_dot_postgresql user.tgz        root.crt $(cred_files "${unpriv_db_users[@]}") \
    || exit 1

echo
echo "All credential files and archives created successfully."
