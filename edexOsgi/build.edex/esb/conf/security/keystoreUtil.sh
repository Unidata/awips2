#!/bin/bash

securityDir=/awips2/edex/conf/security
securityProps=$securityDir/security.properties
publicKeyFile=PublicKey.cer
keystore=keystore.jks
truststore=truststore.jks

keystorePw=
keyAlias=$(hostname -s)
keyPw=

truststorePw=password

function usage {
	echo "Usage:"
	echo -e "\t-h\t\tDisplays usage"
	echo -e "\t-g\t\tGenerate keystore, truststore, and security properties file"
	echo -e "\t-a [keyFile]\tAdds a public key to the trust store"
}

function generateKeystores() {

echo "Generating keystores"

if [ ! -d "$securityDir" ]; then
   mkdir $securityDir
fi

while [ -z $keystorePw ];
do
	echo -n "Enter desired password for keystore [$keystore]: "
	read keystorePw
	if [ -z $keystorePw ];
	then
		echo "Keystore password cannot be empty!"
	fi
done

while [ -z $keyAlias ];
do
	echo -n "Enter alias: "
	read keyAlias
	if [ -z $keyAlias ];
	then
		echo "Alias cannot be empty!"
	fi
done

while [ -z $keyPw ];
do
	echo -n "Enter desired password for key [$keyAlias]: "
	read keyPw
	if [ -z $keyPw ];
	then
		echo "Key password cannot be empty!"
	fi
done

while [ -z $truststorePw ];
do
	echo -n "Enter password for trust store [$truststore]: "
	read truststorePw
	if [ -z $truststorePw ];
	then
		echo "TrustStore password cannot be empty!"
	fi
done

cn=$(hostname)

echo "Generating keystore..."
keytool -genkeypair -alias $keyAlias -keypass $keyPw -keystore $keystore -storepass $keystorePw -validity 360 -dname "CN=$cn, OU=AWIPS, O=Raytheon, L=Omaha, ST=NE, C=US" -keyalg RSA 
echo -n "Exporting public key..."
exportOutput=`keytool -exportcert -alias $keyAlias -keystore $keystore -file $keyAlias$publicKeyFile -storepass $keystorePw 2>&1`
echo "Done!"
obfuscatedKeystorePassword=`java -cp /awips2/edex/lib/dependencies/org.eclipse.jetty/jetty-http-7.6.14.v20131031.jar:/awips2/edex/lib/dependencies/org.eclipse.jetty/jetty-util-7.6.14.v20131031.jar org.eclipse.jetty.util.security.Password $keystorePw 2>&1 | grep OBF`

echo "Generating trust store..."

keytool -genkey -alias tmp -keypass tempPass -dname CN=foo -keystore $truststore -storepass $truststorePw
keytool -delete -alias tmp -keystore $truststore -storepass $truststorePw
keytool -import -trustcacerts -file $keyAlias$publicKeyFile -alias $keyAlias -keystore $truststore -storepass $truststorePw

obfuscatedTruststorePassword=`java -cp /awips2/edex/lib/dependencies/org.eclipse.jetty/jetty-http-7.6.14.v20131031.jar:/awips2/edex/lib/dependencies/org.eclipse.jetty/jetty-util-7.6.14.v20131031.jar org.eclipse.jetty.util.security.Password $truststorePw 2>&1 | grep OBF`


echo -n "Generating security properties file..."

echo "edex.security.keystore.path=$securityDir/$keystore" > $securityProps
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
echo "org.apache.ws.security.crypto.merlin.keystore.password=$obfuscatedKeystorePassword" >> $securityProps
echo "org.apache.ws.security.crypto.merlin.keystore.type=JKS" >> $securityProps
echo "org.apache.ws.security.crypto.merlin.keystore.alias=$keyAlias" >> $securityProps

echo "Done!"

echo -n "Moving key store and trust store to [$securityDir] ..."
mv $truststore $keystore $securityDir
echo "Done!"

echo "Keystores are located at $securityDir"
echo "The public key for this server is located at $(pwd)/$keyAlias$publicKeyFile"
echo "This file may be disseminated to other registry federation memebers who wish to interact with this server"

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
keytool -import -trustcacerts -file $keyfile -alias $userAlias -keystore $securityDir/$truststore

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
elif [ "$1" = "-usage" ] || [ "$1" = "--help" ] || [ "$1" = "-h" ]
then
	usage
	exit 0
fi