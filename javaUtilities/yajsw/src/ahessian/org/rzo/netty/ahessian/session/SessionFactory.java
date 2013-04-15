package org.rzo.netty.ahessian.session;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import org.rzo.netty.ahessian.Constants;

/**
 * A factory for creating Session objects.
 * Session id generator: taken from apache tomcat.
 */
class SessionFactory
{

	
    /**
     * The default message digest algorithm to use.
     * TODO: enable user configured algorithms
     */
    private static final String DEFAULT_ALGORITHM = "MD5";
	private Random random;
	private String entropy;
	private int	sessionIdLength = 16;

	// number of duplicated session ids - anything >0 means we have problems
	private int duplicates=0;
	private MessageDigest	digest;
	
	/** Assignment of id to sessions, to avoid duplicates */
	private static Map<String, Session> _sessions = Collections.synchronizedMap(new HashMap<String, Session>());
	
	/**
	 * Creates a new Session object.
	 * If the given id is null a new id is generated
	 * TODO handle id duplicates
	 * 
	 * @param id the given id
	 * 
	 * @return the session object
	 */
	public Session createSession(String id)
	{
		if (id == null)
			id = generateSessionId();
		Session session = new SessionImpl(id);
		_sessions.put(id, session);
		return session;
	}


	/**
     * Generate and return a new session identifier.
     * Taken from Tomcat
     * TODO 
     */
    private synchronized String generateSessionId() {

        byte random[] = new byte[16];
        String jvmRoute = getJvmRoute();
        String result = null;

        // Render the result as a String of hexadecimal digits
        StringBuffer buffer = new StringBuffer();
        do {
            int resultLenBytes = 0;
            if (result != null) {
                buffer = new StringBuffer();
                duplicates++;
            }

            while (resultLenBytes < this.sessionIdLength) {
                getRandomBytes(random);
                random = getDigest().digest(random);
                for (int j = 0;
                j < random.length && resultLenBytes < this.sessionIdLength;
                j++) {
                    byte b1 = (byte) ((random[j] & 0xf0) >> 4);
                    byte b2 = (byte) (random[j] & 0x0f);
                    if (b1 < 10)
                        buffer.append((char) ('0' + b1));
                    else
                        buffer.append((char) ('A' + (b1 - 10)));
                    if (b2 < 10)
                        buffer.append((char) ('0' + b2));
                    else
                        buffer.append((char) ('A' + (b2 - 10)));
                    resultLenBytes++;
                }
            }
            if (jvmRoute != null) {
                buffer.append('.').append(jvmRoute);
            }
            result = buffer.toString();
        } while (_sessions.containsKey(result));
        return (result);

    }

	private MessageDigest getDigest()
	{
        if (this.digest == null) {
            try
			{
				this.digest = MessageDigest.getInstance(DEFAULT_ALGORITHM);
			}
			catch (NoSuchAlgorithmException e)
			{
				Constants.ahessianLogger.warn("", e);
			}
        }
        return digest;

	}

	private String getJvmRoute()
	{
		return System.getProperty("jvmRoute");
	}
	
    private void getRandomBytes(byte bytes[]) {
        getRandom().nextBytes(bytes);
    }
    
    /**
     * Return the random number generator instance we should use for
     * generating session identifiers.  If there is no such generator
     * currently defined, construct and seed a new one.
     * 
     * @return the random
     */
    private Random getRandom() {
        if (this.random == null) {
            // Calculate the new random number generator seed
            long seed = System.currentTimeMillis();
            long t1 = seed;
            char entropy[] = getEntropy().toCharArray();
            for (int i = 0; i < entropy.length; i++) {
                long update = ((byte) entropy[i]) << ((i % 8) * 8);
                seed ^= update;
            }
                this.random = new java.util.Random();
                this.random.setSeed(seed);
        }
        
        return (this.random);

    }
    
    /**
     * Return the entropy increaser value, or compute a semi-useful value
     * if this String has not yet been set.
     * 
     * @return the entropy
     */
    private String getEntropy() {

        // Calculate a semi-useful value if this has not been set
        if (entropy == null) {
                entropy = this.toString();
            }
        return entropy;
    }
    
    /**
     * Gets the session-id length.
     * 
     * @return the session id length
     */
    public int getSessionIdLength()
	{
		return sessionIdLength;
	}
    
    /**
     * Gets the session for a given id.
     * 
     * @param id the id
     * 
     * @return the session
     */
    public Session getSession(String id)
    {
    	return _sessions.get(id);
    }
    
    public Session removeSession(String id)
    {
    	return _sessions.remove(id);
    }




}
