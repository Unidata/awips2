/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.rcm.oldio;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.net.SocketAddress;
import java.util.LinkedList;

import com.raytheon.rcm.server.Log;


/**
 * Manages a single TCP connection to an RPG upon which the TCM protocol is
 * stacked.
 */
public class TcmConnection {
	public static final int CONNECT_TIMEOUT = 15 * 1000;
	/** <p>The maximum amount of time to wait for more data to be received 
	 * while reading a message.  To specify how long to wait for a for a 
	 * new message, use {@link #setMessageReceiveTimeout(int)}.
	 * 
	 * <p>This is the same value used in AWIPS 1. 
	 */
	public static final int READ_TIMEOUT = 15 * 1000; // Used in AWIPS 1
	
	// The size of a RPG message is in the first 12 bytes
	protected static final int FIRST_PART_SIZE = 12;

	protected static final int MAX_TCM_BODY_SIZE = 10240;
	protected static final int TCM_LOGIN = 0;
	protected static final int TCM_LOGIN_ACK   = 1;
	protected static final int TCM_DATA        = 2;
	protected static final int TCM_DATA_ACK    = 3;
	protected static final int TCM_KEEP_ALIVE  = 4;
	protected static final int TCM_ID = 20983610; // Magic number for login
	
	/* From 2620002E ICD: {When a user application on one side of the PVC
	 * needs determine whether the line is still OK, it sends this message
	 * with a non-zero "init" field. A user application on either side, upon
	 * receiving this message, should respond by the same message with 
	 * "init" reset to 0.
	 */
	protected static final int KEEP_ALIVE_INITIATE = 1;
	protected static final int KEEP_ALIVE_RESPONSE = 0;
	
	// If this message is found in the send queue, send a keep alive
    private static final byte[] KEEP_ALIVE = new byte[0];

	protected Link link;
	protected Socket socket;
	protected LinkedList<byte[]> sendQueue = new LinkedList<byte[]>();
	protected boolean running;
	
	protected DataInputStream ins;
	protected DataOutputStream outs;

	protected Thread readThread, writeThread;

	protected int bodySize;
	protected byte[] firstPart = new byte[FIRST_PART_SIZE];
	private int dataToAck;
    private int messageReceiveTimeout;

	@Deprecated
	private TcmConnection() {
		// unused
	}
	
	public TcmConnection(Link link) {
		this.link = link;
	}
	
	public void startThreads(String nameSuffix) {
		readThread = new Thread(new Runnable() {

			public void run() {
				readProc();
			}
			
		}, "TCM Read " + nameSuffix);
		
		writeThread = new Thread(new Runnable() {

			public void run() {
				writeProc();
			}
			
		}, "TCM Write " + nameSuffix);
	
		running = true;
		
		readThread.setDaemon(true);
		writeThread.setDaemon(true);
		readThread.start();
		writeThread.start();
	}
	
	public void stopThreads() {
		synchronized (sendQueue) {
			if (running) { 
				running = false;
				sendQueue.notify();
			}
		}
	}
	
	public void connect(SocketAddress sa) throws IOException {
		socket = new Socket();
		socket.connect(sa, CONNECT_TIMEOUT);
		ins = new DataInputStream(new BufferedInputStream(socket.getInputStream()));
		outs = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()));
	}
	
	public void close() {
		if (socket == null)
			return;
		try {
			socket.close();
		} catch (IOException e) {
			// nothing
		}
	}
	
	/* Sends a message before any other queued messages.  This is needed
	 * because Link needs to send the sign-on message before any other message
	 * that may have been queued due a connect-on-demand request.  If there
	 * ends up being some kind of queue outside of TcmConnection, this can
	 * be removed.
	 */
	void sendMessageFirst(byte[] buffer) {
		synchronized (sendQueue) {
			sendQueue.addFirst(buffer);
			sendQueue.notify();
		}
	}

	public void sendMessage(byte[] buffer) {
		synchronized (sendQueue) {
			sendQueue.addLast(buffer);
			sendQueue.notify();
		}
	}
	
	public void sendKeepAlive() {
	    sendMessage(KEEP_ALIVE);
	}
	
	protected void readProc() {
		try {
			try {
				while (true)
					link.getManager().notifyMessage(link.getConfig().getRadarID(), 
							readMessage());
			} catch (IOException e) {
			    if (! link.isTeardownInProgress())
			        Log.errorf("%s: I/O error: %s", link.getConfig().getRadarID(), e);
			}
		} finally {
			link.teardown();
		}
	}
	
	protected void writeProc() {
		Log.debugf("writeProc start");
		try {
			try {
				while (true) {
					byte[] msg = null;

					synchronized (sendQueue) {
						while (running && sendQueue.isEmpty()) 
							sendQueue.wait();
						if (! running)
							return;
						msg = sendQueue.pollFirst();
					}
					
					if (msg != KEEP_ALIVE) {
					    Log.debugf("writeProc sending message");
					    writeData(msg);
					} else {
					    Log.debugf("writeProc sending keep-alive");
                        /* There is no requirement that specifies how long to
                         * wait for a response, so there is no code that
                         * actually checks for a timeout.
                         * Just sending the keep-alive is helpful because the
                         * TCP layer will detect some errors.
                         */
					    writeTcmPacket(TCM_KEEP_ALIVE, KEEP_ALIVE_INITIATE, null);
					}
				}
			} catch (InterruptedException e) {
				Log.error("writeProc interrupted");
			} catch (IOException e) {
			    if (! link.isTeardownInProgress())
			        Log.errorf("%s: I/O error: %s", link.getConfig().getRadarID(), e);
			}
		} finally {
			Log.debugf("writeProc exit");
			link.teardown();
		}
	}
	
	public byte[] readMessage() throws IOException {
		socket.setSoTimeout(messageReceiveTimeout);

		while (readTcmHeader() != TCM_DATA)
			readTcmBody(null, 0, bodySize);
		
		socket.setSoTimeout(READ_TIMEOUT);

		readTcmBody(firstPart, 0, firstPart.length);
		int messageSize = ((firstPart[8] & 0xff) <<24) | 
			((firstPart[9] & 0xff) <<16) | ((firstPart[10] & 0xff)<<8) | 
			((firstPart[11] & 0xff));
		byte[] msg = new byte[messageSize];
		System.arraycopy(firstPart, 0, msg, 0, firstPart.length);
		int offset = firstPart.length + 
			readTcmBody(msg, firstPart.length, bodySize);
		
		while (offset < messageSize)
			if (readTcmHeader() == TCM_DATA)
				offset += readTcmBody(msg, offset, bodySize);
			else
				readTcmBody(null, 0, bodySize);

		// Assumes next Nexrad message starts with a new TCM message
		
		return msg;
	}
	
	protected int readTcmHeader() throws IOException {
		int type = ins.readInt();
		int special = ins.readInt();
		bodySize = ins.readInt();
		
		if ((bodySize & 0x80000000) != 0)
			throw new IOException("Protocol Error: TCM compression is not supported");
		
		if (type == TCM_KEEP_ALIVE && special != KEEP_ALIVE_RESPONSE) {
			Log.debugf("%s: got keep alive", link.getConfig().getRadarID());
			writeTcmPacket(TCM_KEEP_ALIVE, KEEP_ALIVE_RESPONSE, null);
		} else	if (type == TCM_DATA && special != 0)
			// Sending ACK before we read the body makes the other side hang up.
			dataToAck = special;
		
		return type;
	}
	
	protected int readTcmBody(byte[] buffer, int ofs, int size) throws IOException {
		if (size <= 0)
			return 0;
		if (size > bodySize)
			throw new IOException("Protocol error: TCM message too short");
		if (buffer != null)
			ins.readFully(buffer, ofs, size);
		else
			ins.skip(size);
		bodySize -= size;
		if (bodySize == 0 && dataToAck != 0) {
			writeTcmPacket(TCM_DATA_ACK, dataToAck, null);
			dataToAck = 0;
		}
		return size;
	}
	
	public void writeData(byte[] data) throws IOException {
	    /* While the client is expected to reassemble fragmented messages,
	     * the server cannot do the same.  We just send everything in on
	     * chunk.
	     */
	    writeTcmPacketUnlimited(TCM_DATA, 0, data);
	    /*
		int pos = 0;
		int limit = data.length;
		while (pos < limit) {
			// It is not clear if we really need to limit the TCM message size
			int now = limit - pos < MAX_TCM_BODY_SIZE ? limit - pos : MAX_TCM_BODY_SIZE;
			writeTcmPacket(TCM_DATA, 0, // 0 == do not send ACKs 
			        data, pos, now);
			pos += now;
		}
		*/
	}
	
	/*
	 * Caller must append trailing NUL to loginStr 
	 */
	public void sendLogin(byte[] loginStr) throws IOException {
		writeTcmPacket(TCM_LOGIN, TCM_ID, loginStr);
	}
	
	public void waitLoginAck() throws IOException {
		socket.setSoTimeout(READ_TIMEOUT);
		// TODO: overall timeout
		int type;
		do {
			type = readTcmHeader();
			readTcmBody(null, 0, bodySize); // We ignore the confirmation message
		} while (type != TCM_LOGIN_ACK);
	}

	protected void writeTcmPacket(int type, int special, byte[] payload) throws IOException {
	    writeTcmPacketUnlimited(type, special, payload);
		//writeTcmPacket(type, special, payload, 0, payload != null ? payload.length : 0);
	}
	
	protected void writeTcmPacket(int type, int special, byte[] payload, int ofs, int len) throws IOException {
		if (payload != null && len > MAX_TCM_BODY_SIZE)
			throw new IllegalArgumentException("TCM message body exceeds 10240 byte limit");

		synchronized (outs) {
			outs.writeInt(type);
			outs.writeInt(special);
			outs.writeInt(len);
			if (len != 0)
				outs.write(payload, ofs, len);
			outs.flush();
		}
	}
	
    protected void writeTcmPacketUnlimited(int type, int special, byte[] payload) throws IOException {
        synchronized (outs) {
            outs.writeInt(type);
            outs.writeInt(special);
            if (payload != null) {
                outs.writeInt(payload.length);
                outs.write(payload);
            } else
                outs.writeInt(0);
            outs.flush();
        }
    }

    public int getMessageReceiveTimeout() {
        return messageReceiveTimeout;
    }

    public void setMessageReceiveTimeout(int messageReceiveTimeout) {
        this.messageReceiveTimeout = messageReceiveTimeout;
    }
	
}
