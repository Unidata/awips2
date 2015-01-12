/**
 * This code has been developed by NWS/OST to support AWIPS II
 * 
 */
package gov.noaa.nws.ost.edex.plugin.binlightning;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.Key;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.crypto.spec.IvParameterSpec;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * BinLightningAESKey
 * 
 * Simple representation of bin lightning AES encryption key and its associated
 * key aliases in the keystore
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20130503      DCS 112   Wufeng Zhou To handle both the new encrypted data and legacy bit-shifted data
 * Jun 03, 2014 3226       bclement    moved from com.raytheon.edex.plugin.binlightning to gov.noaa.nws.ost.edex.plugin.binlightning
 * Jun 09, 2014 3226       bclement    refactored to support multiple stores for different data types
 * Jun 19, 2014 3226       bclement    added getInitializationVector()
 * 
 * </pre>
 * 
 * @author Wufeng Zhou
 * 
 */
public class BinLightningAESKey {
	/** Default location to search for BinLightningAESKey.properties file, and keystore file (normally binLightningAESKeystore.jce as configured in properties file)  */
	public static final String DEFAULT_KEYSTORE_LOC = "/usr/local/ldm";
	
	/** System property name that can used to specify configuration property file, which will overwrite the default keystore location */
	public static final String SYS_PROP_FOR_CONF_FILE = "binlightning.aeskeypropfile";
	
	
    public static final String KEYSTORE_PROP_SUFFIX = ".AESKeystore";

    public static final String KEYSTORE_PASS_PROP_SUFFIX = ".AESKeystorePassword";

    public static final String CIPHER_ALGORITHM_SUFFIX = ".cipherAlgorithm";

    public static final String CIPHER_INITIALIZATION_VECTOR_SUFFIX = ".initializationVectorFile";

    public static final String DEFAULT_CIPHER_ALGORITHM = "AES";

	private static final String CONF_PROPERTIES_FILE = "BinLightningAESKey.properties";  
	public static final String KEY_ALIAS_PREFIX = "^\\d{4}-\\d{2}-\\d{2}";
    private static final Pattern KEY_ALIAS_PREFIX_PATTERN = Pattern.compile(KEY_ALIAS_PREFIX);
    private static final SimpleDateFormat KEY_ALIAS_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");
	
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(BinLightningAESKey.class);

    private static final Properties props = new Properties();
    private static volatile boolean propsLoaded = false;

	private static KeyStore keystore; 
	
    private static final Map<String, BinLightningAESKey[]> keyCache = new ConcurrentHashMap<String, BinLightningAESKey[]>(
            2);

    /**
     * Helper method to selectively get all the bin lightning related AES
     * encryption keys for a lightning type, ordered by key issue date in
     * descending order. Keys will be ignored when its alias is not starting
     * with yyyy-MM-dd prefix or key algorithm is not "AES"
     * 
     * If properties file is specified through system property
     * binlightning.aeskeypropfile, then use it to load the properties.
     * Otherwise, load the default property that is at the same place as this
     * class, and overwrite properties if the property is specified through
     * system property. So, binlightning.aeskeypropfile has higher priority, if
     * it is specified, other properties specified in system property will be
     * ignored
     * 
     * Assumption: Valid key imported/stored to the keystore will have
     * yyyy-MM-dd prefix in its alias.
     * 
     * @param propertyPrefix
     *            prefix for properties associated with a particular lightning
     *            data type
     * @return valid bin lightning AES keys (with aliases) in descending order
     *         of key issue date or null when no valid keys found
     */
    public static BinLightningAESKey[] getBinLightningAESKeys(
            String propertyPrefix) {
        BinLightningAESKey[] rval = keyCache.get(propertyPrefix);
        if (rval == null) {
            if (!propsLoaded) {
                synchronized (props) {
                    if (!propsLoaded) {
                        loadProperties();
                    }
                }
            }
            synchronized (keyCache) {
                rval = keyCache.get(propertyPrefix);
                if (rval == null) {
                    rval = createAESKeys(propertyPrefix);
                    if (rval != null) {
                        keyCache.put(propertyPrefix, rval);
                    }
                }
            }
        }
        return rval;
	}

    /**
     * load properties files. If properties file is specified through system
     * property binlightning.aeskeypropfile, then use it to load the properties.
     * Otherwise, load the default property that is at the same place as this
     * class, and overwrite properties if the property is specified through
     * system property. So, binlightning.aeskeypropfile has higher priority, if
     * it is specified, other properties specified in system property will be
     * ignored
     */
    private static void loadProperties() {
        /*
         * if properties file is specified through system property
         * binlightning.aeskeypropfile, then use it to load the properties
         * otherwise, use default property file and overwrite with available
         * system properties
         */
        try {
            String confFileName = System.getProperty(SYS_PROP_FOR_CONF_FILE);
            if (confFileName != null && !confFileName.isEmpty()) {
                File file = new File(confFileName);
                if (file.exists() == false) {
                    logger.error("System specified property file "
                            + file.getAbsolutePath() + " does not exist.");
                } else {
                    FileInputStream fis = new FileInputStream(file);
                    props.load(fis);
                    fis.close();
                }
            } else {
                // load default properties file
                Properties defProps = new Properties();
                File file = new File(DEFAULT_KEYSTORE_LOC, CONF_PROPERTIES_FILE);
                if (file.exists() == false) {
                    logger.error("Default properties file "
                            + file.getAbsolutePath() + " does not exist.");
                } else {
                    FileInputStream fis = new FileInputStream(file);
                    defProps.load(fis);
                    fis.close();
                }
                props.putAll(defProps);

                /*
                 * now check if the properties should be overwritten, if it is
                 * specified in system properties
                 */
                Iterator<?> iter = defProps.keySet().iterator();
                while (iter.hasNext()) {
                    String key = (String) iter.next();
                    if (System.getProperty(key, "").equals("") == false) {
                        props.setProperty(key, System.getProperty(key));
                    }
                }
            }
            propsLoaded = true;
        } catch (IOException ioe) {
            logger.error(
                    "Fail to load BinLightningAESCipher configuration from file or system properties.",
                    ioe);
            propsLoaded = false;
        }
    }

    /**
     * Read all keys in keystore associated with datatype, ordered by key issue
     * date in descending order. Keys will be ignored when its alias is not
     * starting with yyyy-MM-dd prefix or key algorithm is not "AES"
     * 
     * @param propertyPrefix
     *            prefix for properties associated with a particular lightning
     *            data type
     * @return all keys sorted by
     */
    private static BinLightningAESKey[] createAESKeys(String propertyPrefix) {
        // load keystore
        String keystoreProp = propertyPrefix + KEYSTORE_PROP_SUFFIX;
        String keystorePassProp = propertyPrefix + KEYSTORE_PASS_PROP_SUFFIX;
        BinLightningAESKey[] rval = null;
        try {
            String ksFileName = props.getProperty(keystoreProp);
            if (ksFileName != null && !ksFileName.isEmpty()) {
                File ksFile = new File(ksFileName);
                /*
                 * type JCEKS can store AES symmetric secret key, while default
                 * JKS store can't
                 */
                keystore = KeyStore.getInstance("JCEKS");
                FileInputStream fis = null;
                char[] keystorePassword = null;
                try {
                    fis = new FileInputStream(ksFile);
                    if (props.getProperty(keystorePassProp) != null) {
                        keystorePassword = props.getProperty(keystorePassProp)
                                .toCharArray();
                    }
                    keystore.load(fis, keystorePassword);
                } finally {
                    if (fis != null)
                        fis.close();
                }

                Enumeration<String> enu = keystore.aliases();
                TreeMap<String, Key> treeMap = new TreeMap<String, Key>();
                while (enu.hasMoreElements()) {
                    String alias = enu.nextElement();
                    Matcher matcher = KEY_ALIAS_PREFIX_PATTERN.matcher(alias);
                    /* alias starts with yyyy-MM-dd pattern */
                    if (matcher.lookingAt()) {
                        Key key = keystore.getKey(alias, keystorePassword);
                        if (key.getAlgorithm().equals("AES")) {
                            // valid AES key for bin lightning decryption
                            treeMap.put(alias, key);
                        }
                    }
                }
                List<BinLightningAESKey> keyListSortedByAliasDesc = new ArrayList<BinLightningAESKey>();
                for (Entry<String, Key> entry = treeMap.pollLastEntry(); entry != null; entry = treeMap
                        .pollLastEntry()) {
                    Date keyDate = KEY_ALIAS_DATE_FORMAT.parse(entry.getKey()
                            .substring(0, 10));
                    BinLightningAESKey blkey = new BinLightningAESKey(
                            entry.getKey(), entry.getValue(), keyDate);
                    keyListSortedByAliasDesc.add(blkey);
                }
                rval = keyListSortedByAliasDesc
                        .toArray(new BinLightningAESKey[] {});
            } else {
                logger.error("binlightning.AESKeystore property not set.");
            }
        } catch (KeyStoreException kse) {
            logger.error("Fail to getInstance of JCEKS keystore.", kse);
        } catch (FileNotFoundException fnfe) {
            logger.error(
                    "Fail to find the keystore file configured: "
                            + props.getProperty(keystoreProp), fnfe);
        } catch (NoSuchAlgorithmException e) {
            logger.error(
                    "NoSuchAlgorithmException in loading keystore from file: "
                            + props.getProperty(keystoreProp), e);
        } catch (CertificateException e) {
            logger.error("CertificateException in loading keystore from file: "
                    + props.getProperty(keystoreProp), e);
        } catch (IOException e) {
            logger.error(
                    "IOException in loading keystore from file: "
                            + props.getProperty(keystoreProp), e);
        } catch (UnrecoverableKeyException e) {
            logger.error(
                    "UnrecoverableKeyException in loading keystore from file: "
                            + props.getProperty(keystoreProp), e);
        } catch (ParseException e) {
            logger.error("ParseException in parsing alias for key date: "
                    + props.getProperty(keystoreProp), e);
        }
        return rval;
    }

	/**
	 * force to reload keys, useful for testing
	 * @return
	 */
    public static BinLightningAESKey[] reloadBinLightningAESKeys(
            String propertyPrefix) {
        keyCache.clear();
        propsLoaded = false;
        return getBinLightningAESKeys(propertyPrefix);
	}
	
    /**
     * Get cipher algorithm name for data type
     * 
     * @param propertyPrefix
     *            prefix for properties associated with a particular lightning
     *            data type
     * @return default algorithm if no property is found for prefix
     */
    public static String getCipherAlgorithm(String propertyPrefix) {
        return props.getProperty(propertyPrefix + CIPHER_ALGORITHM_SUFFIX,
                DEFAULT_CIPHER_ALGORITHM);
    }

    /**
     * Create a Cipher initialization vector parameter spec for data type
     * 
     * @param propertyPrefix
     *            prefix for properties associated with a particular lightning
     *            data type
     * @return null if not found or error occurred
     */
    public static IvParameterSpec getInitializationVector(String propertyPrefix) {
        IvParameterSpec rval = null;
        String ivFileName = props.getProperty(propertyPrefix
                + CIPHER_INITIALIZATION_VECTOR_SUFFIX);
        if (ivFileName != null) {
            Path ivPath = Paths.get(ivFileName);
            try {
                byte[] ivData = Files.readAllBytes(ivPath);
                rval = new IvParameterSpec(ivData);
            } catch (IOException e) {
                logger.error(
                        "Unable to create initialization vector for type: "
                        + propertyPrefix, e);
            }
        }
        return rval;
    }
	
	private String alias;
	private Key key;
	private Date keyDate;

	public BinLightningAESKey(String alias, Key key, Date keyDate) {
		this.alias = alias;
		this.key = key;
		this.keyDate = keyDate;
	}
	
	/**
	 * @return the alias
	 */
	public String getAlias() {
		return alias;
	}
	/**
	 * @param alias the alias to set
	 */
	public void setAlias(String alias) {
		this.alias = alias;
	}
	/**
	 * @return the key
	 */
	public Key getKey() {
		return key;
	}
	/**
	 * @param key the key to set
	 */
	public void setKey(Key key) {
		this.key = key;
	}

	public Date getKeyDate() {
		return keyDate;
	}

	public void setKeyDate(Date keyDate) {
		this.keyDate = keyDate;
	}

	public static Properties getProps() {
		return props;
	}
	
}
