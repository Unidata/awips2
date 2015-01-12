/**
 * This code has been developed by NWS/OST to support AWIPS II
 * 
 */
package gov.noaa.nws.ost.edex.plugin.binlightning;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * EncryptedBinLightningCipher
 * 
 * Use AES secret keys found in configured keystore to decrypt bin lightning
 * data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20130503        DCS 112 Wufeng Zhou To handle both the new encrypted data and legacy bit-shifted data
 * Jun 03, 2014 3226       bclement    moved from com.raytheon.edex.plugin.binlightning to gov.noaa.nws.ost.edex.plugin.binlightning
 *                                      handled null return from BinLightningAESKey.getBinLightningAESKeys()
 * Jun 09, 2014 3226       bclement    refactored to support multiple stores for different data types
 * Jun 19, 2014 3226       bclement    added validator callback, added initialization vector support
 * 
 * </pre>
 * 
 * @author Wufeng Zhou
 * 
 */
public class EncryptedBinLightningCipher {

	/** Maximum size of the encrypted block, determined by 3 byte length field in the header */
	private static final int MAX_SIZE_ENCRYPTED_BLOCK = 0xffffff;
	    
    private static final Map<String, Map<String, Cipher>> decryptCipherMapCache = new ConcurrentHashMap<String, Map<String, Cipher>>(
            2);
	
    /**
     * Get cipher map using cache
     * 
     * @param propertyPrefix
     *            datatype properties file prefix
     * @return
     */
    private static Map<String, Cipher> getCachedCipherMap(String propertyPrefix) {
        Map<String, Cipher> rval = decryptCipherMapCache.get(propertyPrefix);
        if (rval == null) {
            synchronized (decryptCipherMapCache) {
                if (rval == null) {
                    rval = createCipherMap(propertyPrefix);
                    decryptCipherMapCache.put(propertyPrefix, rval);
                }
            }
        }
        return rval;
    }

    /**
     * Create a mapping key aliases to keys for datatype
     * 
     * @param propertyPrefix
     *            datatype properties file prefix
     * @return
     */
    private static Map<String, Cipher> createCipherMap(String propertyPrefix) {
        /*
         * get AES keys from keystore and create encryption and decryption
         * ciphers from them
         */
        BinLightningAESKey[] keys = BinLightningAESKey
                .getBinLightningAESKeys(propertyPrefix);
        if (keys == null) {
            keys = new BinLightningAESKey[0];
        }
        String algorithm = BinLightningAESKey
                .getCipherAlgorithm(propertyPrefix);
        IvParameterSpec iv = BinLightningAESKey
                .getInitializationVector(propertyPrefix);
        HashMap<String, Cipher> cipherMap = new HashMap<String, Cipher>();
        for (BinLightningAESKey key : keys) {
            try {
                SecretKeySpec skeySpec = (SecretKeySpec) key.getKey();

                Cipher cipher = Cipher.getInstance(algorithm);
                if (iv != null) {
                    cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv);
                } else {
                    cipher.init(Cipher.DECRYPT_MODE, skeySpec);
                }
                cipherMap.put(key.getAlias(), cipher);
            } catch (Exception e) {
                logger.error(
                        "Fail to create decrypt Cipher from key "
                                + key.getAlias(), e);
            }
        }
        return cipherMap;
    }

    private static IUFStatusHandler logger = UFStatus
            .getHandler(EncryptedBinLightningCipher.class);

    public EncryptedBinLightningCipher() {
		
	}
	
    /**
     * decrypt data with AES keys
     * 
     * @param data
     * @param propertyPrefix
     *            prefix for lightning type configuration
     * @param validator
     *            used to validate decrypted data
     * @return
     * @throws IllegalBlockSizeException
     * @throws BadPaddingException
     */
    public byte[] decryptData(byte[] data, String propertyPrefix,
            DecryptedLightningValidator validator)
            throws IllegalBlockSizeException, BadPaddingException,
            BinLightningDataDecryptionException {
        return decryptData(data, null, propertyPrefix, validator);
	}
	
    /**
     * decrypt data with AES keys, using data observation date as a hint to find
     * the best suitable key to try first
     * 
     * @param data
     * @param dataDate
     * @param propertyPrefix
     *            prefix for lightning type configuration
     * @param validator
     *            used to validate decrypted data
     * @return
     * @throws IllegalBlockSizeException
     * @throws BadPaddingException
     */
    public byte[] decryptData(byte[] data, Date dataDate,
            String propertyPrefix, DecryptedLightningValidator validator)
            throws IllegalBlockSizeException, BadPaddingException,
            BinLightningDataDecryptionException {
		if (data == null) {
			throw new IllegalBlockSizeException("Data is null");
		}
		if (data.length == 0) {
			throw new IllegalBlockSizeException("Data is empty");
		}
		if (data.length > MAX_SIZE_ENCRYPTED_BLOCK) {
            throw new IllegalBlockSizeException(
                    "Block size exceeds maximum expected.");
		}
		
        Map<String, Cipher> cipherMap = getCachedCipherMap(propertyPrefix);
		// find the preferred key order to try decryption based on data date
        List<BinLightningAESKey> preferredKeyList = findPreferredKeyOrderForData(
                dataDate, propertyPrefix);
		
		if (preferredKeyList == null || preferredKeyList.size() == 0) {
			throw new BinLightningDataDecryptionException("No AES key found to decrypt data. Please make sure keystore is properly configured with key(s).");
		}
		
		// try to decrypt the data using ciphers in the list until successful
		byte[] decryptedData = null;
		for (int i = 0; i < preferredKeyList.size(); i++) {
            String alias = preferredKeyList.get(i).getAlias();
            Cipher cipher = cipherMap.get(alias);
            if (cipher == null) {
                logger.warn("No cipher found for alias: " + alias);
                continue;
            }
			try {
				decryptedData = cipher.doFinal(data, 0, data.length);
				
				// wrong key will decrypt data into random noise/garbage, so we need to do a sanity check to make sure 
				//   we are decrypting with the right key
                if (!validator.isValid(decryptedData)) {
					throw new BinLightningDataDecryptionException("Decrypted data (" + decryptedData.length + " bytes) with key " 
                                    + alias
                                    + " is not valid keep-alive or binLightning records.",
                            decryptedData);
				}				
                logger.info("Data (" + data.length + " bytes) decrypted to "
                        + decryptedData.length + " bytes with key: " + alias);
				break; // decrypt ok, break out
			} catch (IllegalBlockSizeException e) {
				// ignore exception if not the last, and try next cipher
                logger.info("Fail to decrypt data (" + data.length
                        + " bytes) with key: " + alias + " - " + e.getMessage()
                        + ", will try other available key");
				if (i == (preferredKeyList.size() - 1)) {
					logger.error("Fail to decrypt with all known keys, either data is not encrypted or is invalid: " + e.getMessage()); 
					throw e;
				}
			} catch (BadPaddingException e) {
				// ignore exception if not the last, and try next cipher
                logger.info("Fail to decrypt data (" + data.length
                        + " bytes) with key: " + alias + " - " + e.getMessage()
                        + ", will try other available key");
				if (i == (preferredKeyList.size() - 1)) {
					logger.error("Fail to decrypt with all known keys, either data is not encrypted or is invalid: " + e.getMessage()); 
					throw e;
				}
			} catch (BinLightningDataDecryptionException e) {
				// ignore exception if not the last, and try next cipher
                logger.info("Fail to decrypt data (" + data.length
                        + " bytes) with key: " + alias + " - " + e.getMessage()
                        + ", will try other available key");
				if (i == (preferredKeyList.size() - 1)) {
					logger.error("Fail to decrypt with all known keys, either data is not encrypted or is invalid: " + e.getMessage()); 
					throw e;
				}
			}
		}
        if (decryptedData == null) {
            throw new BinLightningDataDecryptionException(
                    "No ciphers found for data type: " + propertyPrefix);
        }
		return decryptedData;
	}
	
    /**
     * Assuming the best keys to decrypt data should be issued before the data
     * observation date, so if there were many keys issued, this hopefully will
     * reduce the unnecessary decryption tries
     * 
     * @param dataDate
     * @param propertyPrefix
     *            prefix for lightning type configuration
     * @return preferred key list order
     */
    private List<BinLightningAESKey> findPreferredKeyOrderForData(
            Date dataDate, String propertyPrefix) {
        BinLightningAESKey[] binLightningAESKeys = BinLightningAESKey
                .getBinLightningAESKeys(propertyPrefix);
        if (binLightningAESKeys == null || binLightningAESKeys.length < 1) {
            return Collections.emptyList();
        }
        List<BinLightningAESKey> defKeyList =  Arrays.asList(binLightningAESKeys);
		if (dataDate == null) {
			return defKeyList; // use default order
		} 
		
		int preferredKeyIndex = -1;
		for (int i = 0; i < defKeyList.size() - 2; i++) {
			if (dataDate.before(defKeyList.get(i).getKeyDate()) && dataDate.after(defKeyList.get(i+1).getKeyDate())) {
				// found the preferred key at index i+1
				preferredKeyIndex = i+1;
				break;
			}
		}
		if (preferredKeyIndex == -1) { // no preferred keys found for data date, use default order
			return defKeyList;
		} else {
			List<BinLightningAESKey> preferredList = new ArrayList<BinLightningAESKey>();
			preferredList.addAll(defKeyList.subList(preferredKeyIndex, defKeyList.size()));
			preferredList.addAll(defKeyList.subList(0, preferredKeyIndex));
			return preferredList;
		}
	}
		
    /**
     * ensures that the data is the appropriate length for AES decryption.
     * Copies data to new array.
     * 
     * @param pdata
     * @param traceId
     * @return
     */
    public static byte[] prepDataForDecryption(byte[] pdata, String traceId) {
        /*
         * NOTE: 11/14/2013 WZ:
         * encrypted test data on TNCF (got from Melissa Porricelli)
         * seems to have extra 4 bytes (0x0d 0x0d 0x0a 0x03) at the end,
         * making the data size not a multiple of 16. However, original
         * test data do not have this trailing bytes. while NCEP test
         * data has extra 8 trailing bytes.
         * Brain Rapp's email on 11/13/2013 confirms that Unidata LDM
         * software used by AWIPS II will strips off all SBN protocol
         * headers
         * that precede the WMO header and adds its own 11 byte header
         * like this: "soh  cr  cr  nl   2   5   4  sp  cr  cr  nl". It
         * also adds a four byte trailer consisting of "cr cr nl etx"
         * (0x0d 0x0d 0x0a 0x03)
         * So, it seems necessary to trim trailing bytes if it is not
         * multiple of 16, warning messages will be logged though
         */
        int dataLengthToBeDecrypted = pdata.length;
        if (pdata.length % 16 != 0) {
            dataLengthToBeDecrypted = pdata.length - (pdata.length % 16);
            logger.warn(traceId + " - Data length from file " + traceId
                    + " is " + pdata.length + " bytes, trailing "
                    + (pdata.length - dataLengthToBeDecrypted)
                    + " bytes has been trimmed to " + dataLengthToBeDecrypted
                    + " bytes for decryption.");
        }
        return Arrays.copyOfRange(pdata, 0, dataLengthToBeDecrypted);
    }
}
