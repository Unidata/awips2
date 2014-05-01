/**
 * This code has been developed by NWS/OST to support AWIPS II
 * 
 */
package gov.noaa.nws.ost.edex.plugin.binlightning;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.spec.SecretKeySpec;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * EncryptedBinLightningCipher
 *
 * Use AES secret keys found in configured keystore to decrypt bin lightning data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20130503        DCS 112 Wufeng Zhou To handle both the new encrypted data and legacy bit-shifted data
 * 
 * </pre>
 * 
 * @author Wufeng Zhou
 *
 */
public class EncryptedBinLightningCipher {
	private static final String BINLIGHTNING_CIPHER_TYPE = "AES";

	/** Maximum size of the encrypted block, determined by 3 byte length field in the header */
	private static final int MAX_SIZE_ENCRYPTED_BLOCK = 0xffffff;
	    
    /** 
     * Cipher creation is a relatively expensive operation and would be better to reuse it in the same thread.
     **/
	private static final ThreadLocal<HashMap<String, Cipher>> decryptCipherMap = new ThreadLocal<HashMap<String, Cipher>>() {

		@Override
		protected HashMap<String, Cipher> initialValue() {
			// get AES keys from keystore and create encryption and decryption ciphers from them
			BinLightningAESKey[] keys = BinLightningAESKey.getBinLightningAESKeys();
			HashMap<String, Cipher> cipherMap = new HashMap<String, Cipher>();
			for (BinLightningAESKey key : keys) {
				try {
					SecretKeySpec skeySpec = (SecretKeySpec)key.getKey();
					Cipher cipher = Cipher.getInstance(BINLIGHTNING_CIPHER_TYPE);
					cipher.init(Cipher.DECRYPT_MODE, skeySpec);
					
					cipherMap.put(key.getAlias(), cipher);
				} catch (Exception e) {
					logger.error("Fail to create decrypt Cipher from key " + key.getAlias(), e);
				}
			}
			return cipherMap;
		}		
	};
	
    private static Log logger = LogFactory.getLog(EncryptedBinLightningCipher.class);

    public EncryptedBinLightningCipher() {
		
	}
	
    /**
     * decrypt data with AES keys
     * 
     * @param data
     * @return
     * @throws IllegalBlockSizeException
     * @throws BadPaddingException
     */
	public byte[] decryptData(byte[] data) throws IllegalBlockSizeException, BadPaddingException, BinLightningDataDecryptionException {
		return decryptData(data, null);
	}
	
	/**
	 * decrypt data with AES keys, using data observation date as a hint to find the best suitable key to try first
	 * 
	 * @param data
	 * @param dataDate
	 * @return
	 * @throws IllegalBlockSizeException
	 * @throws BadPaddingException
	 */
	public byte[] decryptData(byte[] data, Date dataDate) throws IllegalBlockSizeException, BadPaddingException, BinLightningDataDecryptionException {
		if (data == null) {
			throw new IllegalBlockSizeException("Data is null");
		}
		if (data.length == 0) {
			throw new IllegalBlockSizeException("Data is empty");
		}
		if (data.length > MAX_SIZE_ENCRYPTED_BLOCK) {
			throw new IllegalBlockSizeException("Block size exceeds maxinum expected.");
		}
		
		HashMap<String, Cipher> cipherMap = EncryptedBinLightningCipher.decryptCipherMap.get();
		// find the preferred key order to try decryption based on data date
		List<BinLightningAESKey> preferredKeyList = findPreferredKeyOrderForData(dataDate);
		
		if (preferredKeyList == null || preferredKeyList.size() == 0) {
			throw new BinLightningDataDecryptionException("No AES key found to decrypt data. Please make sure keystore is properly configured with key(s).");
		}
		
		// try to decrypt the data using ciphers in the list until successful
		byte[] decryptedData = null;
		for (int i = 0; i < preferredKeyList.size(); i++) {
			Cipher cipher = cipherMap.get(preferredKeyList.get(i).getAlias());
			try {
				decryptedData = cipher.doFinal(data, 0, data.length);
				
				// wrong key will decrypt data into random noise/garbage, so we need to do a sanity check to make sure 
				//   we are decrypting with the right key
				if ( BinLigntningDecoderUtil.isKeepAliveRecord(decryptedData) == false && BinLigntningDecoderUtil.isLightningDataRecords(decryptedData) == false) {
				//if (BinLigntningDecoderUtil.isValidMixedRecordData(decryptedData) == false) { // use this only if keep-alive record could be mixed with lightning records
					throw new BinLightningDataDecryptionException("Decrypted data (" + decryptedData.length + " bytes) with key " 
							+ preferredKeyList.get(i).getAlias() + " is not valid keep-alive or binLightning records.", decryptedData);
				}				
				logger.info("Data (" + data.length + " bytes) decrypted to " + decryptedData.length + " bytes with key: " + preferredKeyList.get(i).getAlias()); 
				break; // decrypt ok, break out
			} catch (IllegalBlockSizeException e) {
				// ignore exception if not the last, and try next cipher
				logger.info("Fail to decrypt data (" + data.length + " bytes) with key: " + preferredKeyList.get(i).getAlias() + " - " + e.getMessage() + ", will try other available key"); 
				if (i == (preferredKeyList.size() - 1)) {
					logger.error("Fail to decrypt with all known keys, either data is not encrypted or is invalid: " + e.getMessage()); 
					throw e;
				}
			} catch (BadPaddingException e) {
				// ignore exception if not the last, and try next cipher
				logger.info("Fail to decrypt data (" + data.length + " bytes) with key: " + preferredKeyList.get(i).getAlias() + " - " + e.getMessage() + ", will try other available key"); 
				if (i == (preferredKeyList.size() - 1)) {
					logger.error("Fail to decrypt with all known keys, either data is not encrypted or is invalid: " + e.getMessage()); 
					throw e;
				}
			} catch (BinLightningDataDecryptionException e) {
				// ignore exception if not the last, and try next cipher
				logger.info("Fail to decrypt data (" + data.length + " bytes) with key: " + preferredKeyList.get(i).getAlias() + " - " + e.getMessage() + ", will try other available key"); 
				if (i == (preferredKeyList.size() - 1)) {
					logger.error("Fail to decrypt with all known keys, either data is not encrypted or is invalid: " + e.getMessage()); 
					throw e;
				}
			}
		}
		return decryptedData;
	}
	
	/**
	 * Assuming the best keys to decrypt data should be issued before the data observation date, so 
	 * if there were many keys issued, this hopefully will reduce the unnecessary decryption tries
	 *  
	 * @param dataDate
	 * @return preferred key list order
	 */
	private List<BinLightningAESKey> findPreferredKeyOrderForData(Date dataDate) {
		List<BinLightningAESKey> defKeyList =  Arrays.asList(BinLightningAESKey.getBinLightningAESKeys());
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
		
}
