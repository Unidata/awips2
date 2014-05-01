/**
 * This code has been developed by NWS/OST to support AWIPS II
 * 
 */
package gov.noaa.nws.ost.edex.plugin.binlightning;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
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
import java.util.Map.Entry;
import java.util.Properties;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * BinLightningAESKey
 *
 * Simple representation of bin lightning AES encryption key and its associated key aliases in the keystore
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20130503      DCS 112   Wufeng Zhou To handle both the new encrypted data and legacy bit-shifted data
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
	
	public static final String KEYSTORE_PROP = "binlightning.AESKeystore";
	public static final String KEYSTORE_PASS_PROP = "binlightning.AESKeystorePassword";
	
	private static final String CONF_PROPERTIES_FILE = "BinLightningAESKey.properties";  
	public static final String KEY_ALIAS_PREFIX = "^\\d{4}-\\d{2}-\\d{2}";
    private static final Pattern KEY_ALIAS_PREFIX_PATTERN = Pattern.compile(KEY_ALIAS_PREFIX);
    private static final SimpleDateFormat KEY_ALIAS_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");
	
    private static Log logger = LogFactory.getLog(BinLightningAESKey.class);

	private static Properties props = new Properties();
	private static KeyStore keystore; 
	private static BinLightningAESKey[] keys = null;
	
	/**
	 * Helper method to selectively get all the bin lightning related AES encryption keys, ordered by key issue date in descending order.
	 * Keys will be ignored when its alias is not starting with yyyy-MM-dd prefix or key algorithm is not "AES" 
	 * 
	 * If properties file is specified through system property binlightning.aeskeypropfile, then use it to load the properties.
	 *   Otherwise, load the default property that is at the same place as this class, and overwrite properties
	 *   if the property is specified through system property.
	 * So, binlightning.aeskeypropfile has higher priority, if it is specified, other properties specified in system property will be ignored
	 * 
	 * Assumption: Valid key imported/stored to the keystore will have yyyy-MM-dd prefix in its alias.  
	 * 	  
	 * @return valid bin lightning AES keys (with aliases) in descending order of key issue date
	 *         or null when no valid keys found
	 */
	public static BinLightningAESKey[] getBinLightningAESKeys() {
		if (keys != null) return keys;

		// if properties file is specified through system property binlightning.aeskeypropfile, then use it to load the properties
		// otherwise, use default property file and overwrite with available system properties
		try {
			if (System.getProperty(SYS_PROP_FOR_CONF_FILE, "").equals("") == false) {
				File file = new File(System.getProperty(SYS_PROP_FOR_CONF_FILE));
				if (file.exists() == false) {
					logger.error("System specified property file " + file.getAbsolutePath() + " does not exist.");
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
					logger.error("Default properties file " + file.getAbsolutePath() + " does not exist.");
				} else {
					FileInputStream fis = new FileInputStream(file);
					defProps.load(fis);
					fis.close();
				}						    	
		    	props.putAll(defProps);
		    	
		    	// now check if the properties should be overwritten, if it is specified in system properties
		    	Iterator<?> iter = defProps.keySet().iterator();
		    	while (iter.hasNext()) {
		    		String key = (String)iter.next();
		    		if (System.getProperty(key, "").equals("") == false) {
		    			props.setProperty(key, System.getProperty(key));
		    		}
		    	}
			}
		} catch (IOException ioe) {
			logger.error("Fail to load BinLightningAESCipher configuration from file or system properties.", ioe);
    	}
		
	    // load keystore
		try {
			if (props.getProperty(KEYSTORE_PROP, "").equals("") == false) {
				File ksFile = new File(props.getProperty(KEYSTORE_PROP));
				keystore = KeyStore.getInstance("JCEKS"); // type JCEKS can store AES symmetric secret key, while default JKS store can't
				FileInputStream fis = null;
				try {
					fis = new FileInputStream(ksFile);
					char[] keystorePassword = null;
					if (props.getProperty(KEYSTORE_PASS_PROP) != null) {
						keystorePassword = props.getProperty(KEYSTORE_PASS_PROP).toCharArray();
					}
					keystore.load(fis, keystorePassword);
				} finally {
					if (fis != null) fis.close();
				}
				
				Enumeration<String> enu = keystore.aliases();
				TreeMap<String, Key> treeMap = new TreeMap<String, Key>();
				while (enu.hasMoreElements()) {
					String alias = enu.nextElement();
					Matcher matcher = KEY_ALIAS_PREFIX_PATTERN.matcher(alias);
					if (matcher.lookingAt()) { // alias starts with yyyy-MM-dd pattern
						Key key = keystore.getKey(alias, props.getProperty(KEYSTORE_PASS_PROP).toCharArray());
						if (key.getAlgorithm().equals("AES")) {
							// valid AES key for bin lightning decryption
							treeMap.put(alias, key);
						}
					}
				}
				List<BinLightningAESKey> keyListSortedByAliasDesc = new ArrayList<BinLightningAESKey>();
				for (Entry<String, Key> entry = treeMap.pollLastEntry(); entry != null; entry = treeMap.pollLastEntry()) {
					Date keyDate = KEY_ALIAS_DATE_FORMAT.parse(entry.getKey().substring(0, 10));
					BinLightningAESKey blkey = new BinLightningAESKey(entry.getKey(), entry.getValue(), keyDate);
					keyListSortedByAliasDesc.add(blkey);
				}
				keys = keyListSortedByAliasDesc.toArray(new BinLightningAESKey[] {});
				return keys;
			} else {
				logger.error("binlightning.AESKeystore property not set.");
			}
		} catch (KeyStoreException kse) {
			logger.error("Fail to getInstance of JCEKS keystore.", kse);
		} catch (FileNotFoundException fnfe) {
			logger.error("Fail to find the keystore file configured: " + props.getProperty(KEYSTORE_PROP), fnfe);
		} catch (NoSuchAlgorithmException e) {
			logger.error("NoSuchAlgorithmException in loading keystore from file: " + props.getProperty(KEYSTORE_PROP), e);
		} catch (CertificateException e) {
			logger.error("CertificateException in loading keystore from file: " + props.getProperty(KEYSTORE_PROP), e);			
		} catch (IOException e) {
			logger.error("IOException in loading keystore from file: " + props.getProperty(KEYSTORE_PROP), e);		
		} catch (UnrecoverableKeyException e) {
			logger.error("UnrecoverableKeyException in loading keystore from file: " + props.getProperty(KEYSTORE_PROP), e);		
		} catch (ParseException e) {
			logger.error("ParseException in parsing alias for key date: " + props.getProperty(KEYSTORE_PROP), e);		
		}
		return null;
	}

	/**
	 * force to reload keys, useful for testing
	 * @return
	 */
	public static BinLightningAESKey[] reloadBinLightningAESKeys() {
		if (keys != null) keys = null;
		return getBinLightningAESKeys();
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
