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
package com.raytheon.uf.common.util.cache;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * LRU Cache based on size, similar to LinkedHashMap but with more control
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LRUCache<K, V extends ICacheObject> {

    protected class Item {
        public long lastUsedTime;

        public K key;

        public V value;

        public Item previous;

        public Item next;

        public Item() {
        }

        public Item(K key, V value) {
            this.key = key;
            this.value = value;
        }
    }

    /** The cache metadata map for the cache */
    protected final Map<K, Item> lruMap = Collections
            .synchronizedMap(new HashMap<K, Item>());

    /** The head (first item in the list */
    private final Item head = new Item();

    /** The tail (last item in the list */
    private final Item tail = new Item();

    /** The maximum size of items contained in the cache, in bytes */
    private long maxSize;

    /** The current size of items contained in the cache, in bytes */
    private long curSize;

    public LRUCache(long maxSize) {
        this.maxSize = maxSize;
        curSize = 0;
        this.head.next = this.tail;
        this.tail.previous = this.head;
    }

    private void insertHead(Item item) {
        synchronized (this) {
            item.previous = this.head;
            item.next = this.head.next;
            this.head.next.previous = item;
            this.head.next = item;
        }
    }

    private void moveToHead(Item item) {
        synchronized (this) {
            item.previous.next = item.next;
            item.next.previous = item.previous;
            item.previous = this.head;
            item.next = this.head.next;
            this.head.next.previous = item;
            this.head.next = item;
        }
    }

    public void put(K key, V value) {
        Item cur = this.lruMap.get(key);
        if (cur != null) {
            // set the value and move to head
            curSize -= cur.value.getSize();
            cur.value = value;
            curSize += cur.value.getSize();
            cur.lastUsedTime = System.currentTimeMillis();

            moveToHead(cur);

            // make sure we are still under maxSize
            Item last = null;
            while (this.curSize >= this.maxSize) {
                last = this.tail.previous;
                this.lruMap.remove(last.key);
                removeItem(last);
            }
            return;
        } else {
            // remove if needed
            Item last = null;
            while (this.curSize >= this.maxSize) {
                last = this.tail.previous;
                this.lruMap.remove(last.key);
                removeItem(last);
            }

            Item item = new Item(key, value);
            item.lastUsedTime = System.currentTimeMillis();
            insertHead(item);
            this.curSize += item.value.getSize();
            this.lruMap.put(item.key, item);
        }
    }

    /**
     * Get the item from the cache, move to head
     * 
     * @param key
     * @return
     */
    public V get(K key) {
        Item item = lruMap.get(key);
        if (item != null) {
            item.lastUsedTime = System.currentTimeMillis();
            moveToHead(item);
            return item.value;
        }
        return null;
    }

    /**
     * Remove an image from the cache
     * 
     * @param image
     *            the image to remove
     */
    public void remove(K key) {
        Item cur = this.lruMap.get(key);
        if (cur == null) {
            return;
        }
        this.lruMap.remove(key);
        removeItem(cur);
    }

    protected void removeItem(Item item) {
        synchronized (this) {
            this.curSize -= item.value.getSize();
            item.previous.next = item.next;
            item.next.previous = item.previous;
        }
    }

    /**
     * Return the current size of the elements contained in the cache (in bytes)
     * 
     * @return the size in bytes
     */
    public long size() {
        return this.curSize;
    }

    /**
     * The maximum size of the cache in bytes
     * 
     * @return
     */
    public long maxSize() {
        return this.maxSize;
    }

    @Override
    public String toString() {
        return "Size = " + getSize();
    }

    private String getSize() {
        long size = curSize;
        if (size > 1024 * 1024) {
            return ((int) (size / (1024 * 1024))) + " MB";
        } else if (size > 1024) {
            return ((int) (size / (1024))) + " KB";
        } else {
            return size + " B";
        }
    }
}
