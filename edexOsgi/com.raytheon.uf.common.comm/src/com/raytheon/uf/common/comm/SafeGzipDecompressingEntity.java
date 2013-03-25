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
package com.raytheon.uf.common.comm;

import java.io.IOException;
import java.io.InputStream;
import java.lang.ref.WeakReference;
import java.util.zip.GZIPInputStream;

import org.apache.http.HttpEntity;
import org.apache.http.client.entity.GzipDecompressingEntity;

/**
 * <pre>
 * 
 * This class obsessively closes GZIPInputStreams to end Inflaters
 * and keep native memory free.
 * 
 * Assumptions:
 * This class can only be used if the content in this entity is going to be
 * created and consumed on the same thread. If a new entity is created on the
 * same thread, the content of the previous entity will be closed so don't use
 * if a single thread is managing multiple responses at the same time.
 * 
 * The Back Story:
 * java.util.zip.Inflater is known to hold native memory until it is garbage
 * collected or until end() is
 * called(http://bugs.sun.com/view_bug.do?bug_id=4797189). The default
 * GzipDecompressingEntity uses GZIPInputStream which uses Inflater for
 * decompression. Ideally when the stream is done, it would get closed, which
 * would trigger the Inflater to end and free the native resources. What really
 * happens is that this entity gets wrapped in a BasicManagedEntity which wraps
 * the GZIPInputStream in a EofSensorInputStream and at the end of the day the
 * Inflater is left unended. The Inflater will still get cleaned when garbage
 * collection runs and the finalizer gets invoked, but this adds some ambiguity
 * to the availability of native memory. This calss exists to remove that 
 * ambiguity. There are a few things which can compound the problem and make
 * things worse:
 *  1. When the java heap is excessively large then the garbage collector may 
 *     run less frequently since it has lots of space but it also means there
 *     is less native memory available.
 *  2. When the process is running lots of native code outside the JVM then the
 *     garbage collector will not run as often but the demand for native memory 
 *     will be greater.
 * 
 * 
 * There are three ways that this class keeps the number of open
 * GZIPInputStreams down.
 *  1. Provides a static close method that can be used by HttpClient to force
 *     close GZIPInputStream.
 *  2. Uses a ThreadLocal to limit the number of GZIPInputStreams to one per
 *     thread. This is also the mechanism used to staticly track which
 *     GZIPInputStream need to be closed.
 *  3. Only holds weak references to the GZIPInputStreams so that if the
 *     garbage collector beats us to it then the Inflaters can get freed early.
 * 
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2013            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
class SafeGzipDecompressingEntity extends GzipDecompressingEntity{

    private static ThreadLocal<WeakReference<GZIPInputStream>> stream = new ThreadLocal<WeakReference<GZIPInputStream>>();
    
    public SafeGzipDecompressingEntity(HttpEntity entity) {
        super(entity);
    }

    @Override
    public InputStream getContent() throws IOException {
        close();
        InputStream istream = super.getContent();
        if (istream instanceof GZIPInputStream) {
            // save off a reference to the stream so it can be clsoed later.
            stream.set(new WeakReference<GZIPInputStream>(
                    (GZIPInputStream) istream));
        }
        return istream;
    }
    
    /**
     * Closes whatever GZIPInputStream's are open on this Thread, freeing any
     * native resources used by the Inflater.
     * 
     * @throws IOException
     */
    public static void close() throws IOException {
        WeakReference<GZIPInputStream> ref = stream.get();
        if(ref != null){
            stream.remove();
            GZIPInputStream stream = ref.get();
            if(stream != null){
                stream.close();
            }
        }
    }
    
}