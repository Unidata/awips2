#!/bin/bash
# Temporary, only be used until we get DOD certs.
# rewrite from 16.1.1

SETUP_ENV=/awips2/edex/bin/setup.env
source $SETUP_ENV

if [[ -z $JAR_LIB ]]
then
        JAR_LIB="/awips2/edex/lib"
fi

FIND_JAR_COMMAND="find $JAR_LIB -name *.jar"
JAR_FOLDERS=`$FIND_JAR_COMMAND`

#Recursively search all library directories for jar files and add them to the local classpath
addSep=false
for i in $JAR_FOLDERS;
do
        if [[ "$addSep" == true ]];
        then
                LOCAL_CLASSPATH=$LOCAL_CLASSPATH":"$i
        else
                LOCAL_CLASSPATH=$i
                addSep=true
        fi
done

JAVA_BIN=/awips2/java/jre/bin/java

securityDir=/awips2/edex/conf/security
securityPropertiesDir=/awips2/edex/conf/resources
securityProps=$securityPropertiesDir/security.properties
publicKeyFile=PublicKey.cer
keystore=keystore.jks
truststore=truststore.jks


encryptionKey=encrypt

defaultPassword=password
defaultOrg=UCAR
defaultOrgUnit=Unidata
defaultLoc=Boulder
defaultState=CO
defaultSAN=ip:$(hostname --ip-address|head -1)
infoCorrect=

function resetVariables {
	orgUnit=
	org=
	loc=
	state=
	country=
	ext=
	keystorePw=
	truststorePw=
	keyPw=
	cn=
}

function usage {
	echo "Usage:"
	echo -e "\t-h\t\tDisplays usage"
	echo -e "\t-g\t\tGenerate keystore, truststore, and security properties file"
	echo -e "\t-a [keyFile]\tAdds a public key to the trust store"
	echo -e "\t-d [keyFile]\tDeletes a public key from the trust store"
}

function generateKeystores() {
	

echo "Generating keystores"

if [[ ! -d $securityDir ]]; then
   mkdir $securityDir
fi

if [[ ! -d $securityPropertiesDir ]]; then
   mkdir -p $securityPropertiesDir
fi
	
while [[ $infoCorrect != "yes" ]];
do
	infoCorrect=
	resetVariables
	while [[ -z $keystorePw ]];
	do
		echo -n "Enter password for $keystore [$defaultPassword]: "
		read keystorePw
		if [[ -z $keystorePw ]]; 
		then
			echo -e "\tUsing default password of $defaultPassword"
			keystorePw=$defaultPassword
		elif [[ ${#keystorePw} -lt 6 ]];
		then
			echo -e "\tPassword must be at least 6 characters."
			keystorePw=
		fi
	done
	
	while [[ -z $keyAlias ]];
	do
		if [[ -z $CLUSTER_ID ]] 
		then
			HOST=$(hostname -s)
			CLUSTER_ID=${HOST:$(expr index "$HOST" -)} | tr '[:lower:]' '[:upper:]'
		fi
		
		if [[ -z $CLUSTER_ID ]] 
		then	
			CLUSTER_ID=$AW_SITE_IDENTIFIER
		fi
		
		echo -n "Enter keystore alias [$CLUSTER_ID]: "
		read keyAlias
		if [[ -z $keyAlias ]];
		then
			echo -e "\tUsing default value of $CLUSTER_ID"
			keyAlias=$CLUSTER_ID
		else
			CLUSTER_ID=$keyAlias
		fi
		# Write the cluster ID to the setup.env file
		echo "CLUSTER_ID set to: $CLUSTER_ID"
		sed -i "s@^export CLUSTER_ID.*@export CLUSTER_ID=$CLUSTER_ID@g" $SETUP_ENV
	done
	
	while [[ -z $keyPw ]];
	do
		echo -n "Enter password for key $keyAlias [$defaultPassword]: "
		read keyPw
		if [[ -z $keyPw ]];
		then
			echo -e "\tUsing default password of $defaultPassword"
			keyPw=$defaultPassword
		elif [[ ${#keyPw} -lt 6 ]];
		then
			echo -e "\tPassword must be at least 6 characters."
			keyPw=
		fi
	done
	
	while [[ -z $truststorePw ]];
	do
		echo -n "Enter password for $truststore [$defaultPassword]: "
		read truststorePw
		if [[ -z $truststorePw ]];
		then
			echo -e "\tUsing default password of $defaultPassword"
			truststorePw=$defaultPassword
		elif [[ ${#truststorePw} -lt 6 ]];
		then
			echo -e "\tPassword must be at least 6 characters."
			truststorePw=
		fi
	done
	
	while [ -z $cn ];
	do
		echo -n "Enter canonical name/IP [$(hostname)]: "
		read cn
		if [ -z $cn ];
		then
			echo -e "\tUsing default value of $(hostname)"
			cn=$(hostname)
		fi
	done

	while [[ -z $org ]];
	do
		echo -n "Enter Organization (O) [$defaultOrg]: "
		read org
		if [[ -z $org ]];
		then
			echo -e "\tUsing default value of $defaultOrg"
			org=$defaultOrg
		fi
	done

	while [[ -z $orgUnit ]];
	do
		echo -n "Enter Organizational Unit (OU) [$defaultOrgUnit]: "
		read orgUnit
		if [[ -z $orgUnit ]];
		then
			echo -e "\tUsing default value of $defaultOrgUnit"
			orgUnit=$defaultOrgUnit
		fi
	done
	
	while [[ -z $loc ]];
	do
		echo -n "Enter Location (L) [$defaultLoc]: "
		read loc
		if [[ -z $loc ]];
		then
			echo -e "\tUsing default value of $defaultLoc"
			loc=$defaultLoc
		else
			loc=${loc// /_}
		fi
	done
	
	while [[ -z $state ]];
	do
		echo -n "Enter State (ST) (2 letter ID) [$defaultState]: "
		read state
		if [[ -z $state ]];
		then
			echo -e "\tUsing default value of $defaultState"
			state=$defaultState
		fi
	done
	
	while [[ -z $country ]];
	do
		echo -n "Enter Country (C) (2 letter ID) [US]: "
		read country
		if [[ -z $country ]];
		then
			echo -e "\tUsing default value of US"
			country=US
		fi
	done
	
	while [[ -z $ext ]];
	do
		echo "Subject Alternative Names (SAN): Comma Delimited!"
		echo "for FQDN enter: dns:host1.mydomain.com,dns:host2.mydomain.com, etc"
		echo "for IP enter: ip:X.X.X.X,ip:Y.Y.Y.Y, etc"
		echo -n "Enter SAN [$defaultSAN]: "
		read ext
		if [[ -z $ext ]];
		then
			echo -e "\tUsing default value of $defaultSAN"
			ext=$defaultSAN
		fi
	done
	
	echo
	echo "        ______________Summary______________"
	echo "           Keystore: $securityDir/$keystore"
	echo "  Keystore Password: $keystorePw"
	echo "         Truststore: $securityDir/$truststore"
	echo "Truststore Password: $truststorePw"
	echo "          Key Alias: $keyAlias"
	echo "       Key Password: $keyPw"
	echo "                SAN: $ext"
	echo "                 CN: $cn"
	echo "                  O: $org"
	echo "                 OU: $orgUnit"
	echo "           Location: $loc"
	echo "              State: $state"
	echo "            Country: $country" 
	echo
	
	while [[ $infoCorrect != "yes" ]] && [[ $infoCorrect != "y" ]] && [[ $infoCorrect != "no" ]] && [[ $infoCorrect != "n" ]];
	do
		echo -n "Is this information correct (yes or no)? "
		read infoCorrect
		infoCorrect=$(echo $infoCorrect | tr '[:upper:]' '[:lower:]')
		if [[ $infoCorrect = "yes" ]] || [[ $infocorrect = "y" ]];
		then
			echo "Information Confirmed"
		elif [[ $infoCorrect = "no" ]] || [[ $infoCorrect = "n" ]];
		then
			echo -e "\nPlease re-enter the information."
			resetVariables
		else
			echo "Please enter yes or no."
		fi
	done

done

cn=$(hostname)

echo "Generating keystore..."
# get rid of an existing key with same name
echo "Checking to see if a key with this alias exists in keystore.....[$keyAlias]!"
keytool -delete -alias $keyAlias -keystore $securityDir/$keystore
# create and add key
keytool -genkeypair -alias $keyAlias -keypass $keyPw -keystore $keystore -storepass $keystorePw -validity 360 -dname "CN=$cn, OU=$orgUnit, O=$org, L=$loc, ST=$state, C=$country" -keyalg RSA -ext san=$ext
echo -n "Exporting public key..."
exportOutput=`keytool -exportcert -alias $keyAlias -keystore $keystore -file $keyAlias$publicKeyFile -storepass $keystorePw 2>&1`
echo "Done!"
obfuscatedKeystorePassword=`$JAVA_BIN -cp $LOCAL_CLASSPATH com.raytheon.uf.common.security.encryption.AESEncryptor encrypt $encryptionKey $keystorePw 2>&1`

echo "Generating trust store..."
echo "Checking to see if a trusted CA with this alias exists in truststore.....[$keyAlias]!"
keytool -delete -alias $keyAlias -keystore $securityDir/$truststore
keytool -genkey -alias tmp -keypass tempPass -dname CN=foo -keystore $truststore -storepass $truststorePw
keytool -delete -alias tmp -keystore $truststore -storepass $truststorePw
keytool -import -trustcacerts -file $keyAlias$publicKeyFile -alias $keyAlias -keystore $truststore -storepass $truststorePw

jettyObscuredPassword=`$JAVA_BIN -cp $LOCAL_CLASSPATH org.eclipse.jetty.util.security.Password $keystorePw 2>&1 | grep OBF`

obfuscatedTruststorePassword=`$JAVA_BIN -cp $LOCAL_CLASSPATH com.raytheon.uf.common.security.encryption.AESEncryptor encrypt $encryptionKey $truststorePw 2>&1`


echo -n "Generating security properties file..."

echo "# This file was automatically generated using /awips2/edex/conf/security/keystoreUtil.sh" > $securityProps
echo "java.security.auth.login.config=/awips2/edex/conf/security/realms.properties" >> $securityProps
echo "edex.security.auth.user=$keyAlias" >> $securityProps
echo "edex.security.auth.password=$obfuscatedKeystorePassword" >> $securityProps
echo "edex.security.auth.authorizationType=Basic" >> $securityProps
echo "edex.security.auth.loginService.name=RegistryRealm" >> $securityProps
echo "edex.security.auth.loginService.realm=RegistryRealm" >> $securityProps
echo "edex.security.encryption.key=$encryptionKey" >> $securityProps
echo "edex.security.keystore.path=$securityDir/$keystore" >> $securityProps
echo "edex.security.keystore.alias=$keyAlias" >> $securityProps
echo "edex.security.keystore.password=$obfuscatedKeystorePassword" >> $securityProps
echo "edex.security.keystore.type=JKS" >> $securityProps
echo "edex.security.keystore.algorithm=SunX509" >> $securityProps
echo "edex.security.truststore.path=$securityDir/$truststore" >> $securityProps
echo "edex.security.truststore.password=$obfuscatedTruststorePassword" >> $securityProps
echo "edex.security.truststore.type=JKS" >> $securityProps
echo "edex.security.truststore.algorithm=SunX509" >> $securityProps
echo "edex.security.disableCNCheck=false" >>$securityProps

echo "#The following configuration items are used with the wss4j in/out interceptors" >> $securityProps
echo "org.apache.ws.security.crypto.merlin.keystore.file=security/$keystore" >> $securityProps
echo "org.apache.ws.security.crypto.merlin.keystore.password=$jettyObscuredPassword" >> $securityProps
echo "org.apache.ws.security.crypto.merlin.keystore.type=JKS" >> $securityProps
echo "org.apache.ws.security.crypto.merlin.keystore.alias=$keyAlias" >> $securityProps

echo "Done!"

# If we are already in the security directory, we do not
# need to move the files
if [[ $(pwd) != "$securityDir" ]];
then
	echo -n "Moving key store and trust store to [$securityDir] ..."
	mv $truststore $keystore $securityDir
	echo "Done!"
fi

echo "Keystores are located at $securityDir"
echo "The public key for this server is located at $(pwd)/$keyAlias$publicKeyFile"
echo "This file may be disseminated to other registry federation members who wish to interact with this server"

}

function addKey() {
echo "Adding $keyfile to trust store..."

userAlias=
while [ -z $userAlias ];
do
	echo -n "Enter alias for [$keyfile]: "
	read userAlias
	if [ -z $userAlias ];
	then
		echo "Alias cannot be empty!"
	fi
done

# delete any existing cert in the truststore for this alias
echo "Checking to see if a certificate with this alias exists to replace.....[$userAlias]!"
keytool -delete -alias $userAlias -keystore $securityDir/$truststore
# add the cert as a Self Signed CA to truststore 
keytool -import -trustcacerts -file $keyfile -alias $userAlias -keystore $securityDir/$truststore

}

function deleteKey() {
echo "Deleting $keyfile from trust store..."

userAlias=
while [ -z $userAlias ];
do
	echo -n "Enter alias for [$keyfile]: "
	read userAlias
	if [ -z $userAlias ];
	then
		echo "Alias cannot be empty!"
	fi
done
keytool -delete -alias $userAlias -keystore $securityDir/$truststore

}


if [ $# -eq 0 ]
then
	echo "No arguments supplied"
	usage
	exit 0
elif [ "$1" = "-g" ]
then
	generateKeystores
	exit 0
elif [ "$1" = "-a" ]
then
	if [ $# -lt 2 ]
	then
		echo "No key file supplied"
		usage
	elif [ ! -e $securityDir/$truststore ]
	then
		echo "Trust store [$securityDir/$truststore] does not exist!"
	else
		keyfile=$2
		addKey
	fi
	exit 0
elif [ "$1" = "-d" ]
then
	if [ $# -lt 2 ]
	then
		echo "No key file supplied"
		usage
	elif [ ! -e $securityDir/$truststore ]
	then
		echo "Trust store [$securityDir/$truststore] does not exist!"
	else
		keyfile=$2
		deleteKey
	fi
	exit 0	
elif [ "$1" = "-usage" ] || [ "$1" = "--help" ] || [ "$1" = "-h" ]
then
	usage
	exit 0
fi