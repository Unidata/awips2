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
package com.raytheon.viz.pointdata.rsc.progdisc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.viz.pointdata.rsc.progdisc.GenericProgressiveDisclosure.PlotItem;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A generic progressive disclosure algorithm that will determine a subset of
 * {@link PlotItem}s that should be displayed given a particular {@link IExtent}
 * and the desired distance between plot items. The exact disclosure for a set
 * of items can be weighted by adding the items in a specific order. In general
 * the items that are added first will displayed sooner than those added later.
 * For example if you were displaying cities, then sorting the cities by
 * population before adding would result in large cities being displayed before
 * small cities.
 * <p>
 * This class was designed to scale well for any number of points. To achieve
 * this it uses a combination of two distinct algorithms with different
 * performance characteristics:
 * <ol>
 * <li>The <b>static algorithm</b>: When a new item is inserted using this
 * algorithm then the position is compared to all the existing items to
 * determine the distance from the closest item. When disclosure is run it is
 * simply a loop over the items to find any with a large enough distance value.
 * This has Θ(n²) insertion time, Θ(n) disclosure time.
 * <li>The <b>dynamic algorithm</b>: When a new item is inserted using this
 * algorithm it is simply appended to the list of items. When disclosure is run
 * then each item is compared to any other displayed items to see if it fits
 * into the display without getting to close. This has Θ(1) insertion time,
 * Θ(n*m) disclosure time(m is the number of items actually displayed).
 * <p>
 * The static algorithm is much better for performance if the disclosure is
 * going to be reused since it has a much faster disclosure time. However the
 * longer insertion time can be a problem for more than 5000-10000 items.
 * <p>
 * Besides raw performance, a major consideration between the two algorithms is
 * that the dynamic algorithm suffers from inconsistent disclosure that can
 * cause a flickering artifact as the extent changes. For example if you zoom in
 * on a display then sometimes there is enough room to replace a single item
 * with two items that are spread further apart, however if those items are too
 * close to other items then they may be removed and replaced with items
 * slightly further away. A very small change to the extent leads to a very big
 * change in the set of items displayed which is generally unpleasant for users.
 * <p>
 * There are two tweaks to the dynamic algorithm that help mitigate the flicker.
 * <ol>
 * <li>First, when the dynamic algorithm is used it caches the disclosed items
 * and tries to disclose the same items across multiple calls, before
 * considering new items. This helps immensely because as the extent changes
 * only minimal changes are made to the set of disclosed items. There can still
 * be a problem if you zoom in on an area and zoom back out, the extra items
 * introduced by zooming in can significantly change the result when zooming
 * out. Although this effect is much more gradual than a flicker, it is still
 * annoying when there is a specific item of interest that disappears
 * unexpectedly.
 * <li>Secondly, pure dynamic disclosure is never used, instead a hybrid of
 * dynamic/static is always used. For the first few thousand items the insertion
 * cost for the static algorithm is very low so using a static algorithm for a
 * certain number of points is beneficial. The static items can be quickly
 * disclosed before adding in the dynamic points and they act like anchor points
 * that help keep the dynamic algorithm more consistent.
 * <p>
 * The hybrid approach provides a balance between getting something to display
 * quickly and keeping the disclosure consistent. If the disclosure is going to
 * be used for awhile it may be worthwhile to use the static disclosure more. It
 * is possible to asynchronously calculate the static values so that as soon as
 * possible the disclosure will be completely consistent(
 * {@link #calculateStaticDistances()}.
 * <p>
 * This class uses internal synchronization to make it safe to use from multiple
 * threads. When properly configured all function calls will be very fast so
 * there should not be significant waiting.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Nov 12, 2015  4903     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @param <T>
 */
public class GenericProgressiveDisclosure<T extends PlotItem> {

    private final int staticToDynamicThreshold;

    private AtomicInteger waitingTasks = new AtomicInteger(0);

    private int staticCount = 0;

    private List<ItemInfo<T>> items = new ArrayList<>();

    private List<ItemInfo<T>> prevItems = Collections.emptyList();

    public GenericProgressiveDisclosure() {
        this(5000);
    }

    /**
     * Construct a new instance with a specific threshold for switching between
     * the two disclosure algorithms. Its usually best to keep the number
     * relatively small and use {@link #calculateStaticDistances()}
     * asynchronously to ensure that that
     * {@link #runDisclosure(IExtent, double)} does not have to wait for slow
     * calls to {@link #add(PlotItem)}.
     * 
     * @param staticToDynamicThreshold
     *            the number of items that should calculate static distance
     *            values before switching to dynamic calculations.
     */
    public GenericProgressiveDisclosure(int staticToDynamicThreshold) {
        this.staticToDynamicThreshold = staticToDynamicThreshold;
    }

    /**
     * @return all the {@link PlotItem}s contained in this disclosure.
     */
    public List<T> getAll() {
        ArrayList<T> all = new ArrayList<>();
        waitingTasks.incrementAndGet();
        synchronized (items) {
            all.ensureCapacity(items.size());
            for (ItemInfo<T> info : items) {
                all.add(info.getItem());
            }
        }
        waitingTasks.decrementAndGet();
        return all;
    }

    /**
     * Get a set of items that are all within extent and are not closer together
     * than minDistance.
     * 
     * @param extent
     *            area whcich will contain all items returned
     * @param minDistance
     *            the minimum distance between any two disclosed items, no two
     *            items returned will be closer than this.
     * @return items
     */
    public Collection<T> runDisclosure(IExtent extent, double minDistance) {
        List<ItemInfo<T>> items = new ArrayList<>();
        Set<T> result = new HashSet<>(100);
        waitingTasks.incrementAndGet();
        synchronized (this.items) {
            for (ItemInfo<T> item : getStaticItems()) {
                if (item.getDistance() > minDistance && item.within(extent)) {
                    result.add(item.getItem());
                    items.add(item);
                }
            }
            if (staticCount < this.items.size()) {
                for (ItemInfo<T> item : this.prevItems) {
                    if (!item.hasDistance() && item.within(extent)
                            && getMinDistance(items, item) > minDistance) {
                        if (result.add(item.getItem())) {
                            items.add(item);
                        }
                    }
                }
                for (ItemInfo<T> item : getDynamicItems()) {
                    if (item.within(extent)
                            && getMinDistance(items, item) > minDistance) {
                        if (result.add(item.getItem())) {
                            items.add(item);
                        }
                    }
                }
                this.prevItems = items;
            }
        }
        waitingTasks.decrementAndGet();
        return result;
    }

    /**
     * Add a new item that can be disclosed. This will not do any aggressive
     * duplicate checking.
     */
    public double add(T item) {
        return add(item, false);
    }

    /**
     * Add a new item that can be disclosed.
     * 
     * @param item
     *            the item to add
     * @param checkDuplicate
     *            when this is true then the item will be checked against all
     *            existing items and ignored if it is in a location that is
     *            already used. This slows down the method so if you are
     *            reasonably certain there are no duplicates its better to use
     *            false.
     * @return The static distance between the new item and the next closest
     *         item, or NaN if there are too many items for static calculations.
     */
    public double add(T item, boolean checkDuplicate) {
        ItemInfo<T> info = new ItemInfo<>(item);
        waitingTasks.incrementAndGet();
        synchronized (items) {
            if (items.size() < staticToDynamicThreshold) {
                double distance = getMinDistance(items, info);
                info.setDistance(distance);
            } else if (checkDuplicate) {
                for (ItemInfo<T> itemToCheck : items) {
                    if (itemToCheck.getItem().getLocation()
                            .equals(item.getLocation())) {
                        info.setDistance(0);
                        break;
                    }
                }
            }
            if (!info.hasDistance()) {
                items.add(info);
            } else if (info.getDistance() > 0) {
                staticCount += 1;
                items.add(info);
            }
        }
        waitingTasks.decrementAndGet();
        return info.getDistance();
    }

    /**
     * Convert any items within the disclosure that are using the dynamic
     * algorithm to be able to use the static algorithm. This will result in
     * more consistent disclosure and faster execution of
     * {@link #runDisclosure(IExtent, double)}. If there are alot of items this
     * method can be slow and should not be run on a thread that is sensitive to
     * delays.
     * <p>
     * Running the calculation requires an exclusive lock that can block other
     * methods. to avoid causing problems this method will return early if there
     * is another thread using this object. The early return will often occur
     * after some items have been converted so future calls will have less work
     * to do. If the return value indicates an early return then the calling
     * code should generally {@link Thread#sleep(long)}, {@link Thread#yield()},
     * or do other tasks before calling this method again. Calling this method
     * again before other threads have a chance to run will return immediately
     * after doing no work.
     * 
     * @return false if the calculation was interrupted and there are more
     *         dynamic items, true if this disclosure is now 100% static.
     */
    public boolean calculateStaticDistances() {
        if (waitingTasks.get() > 0) {
            return false;
        }
        synchronized (items) {
            while (staticCount < items.size()) {
                ItemInfo<T> item = items.get(staticCount);
                double distance = getMinDistance(getStaticItems(), item);
                if (distance > 0) {
                    item.setDistance(distance);
                    staticCount += 1;
                } else {
                    items.remove(staticCount);
                }
                if (waitingTasks.get() > 0) {
                    return false;
                }
            }
            return true;
        }
    }

    protected List<ItemInfo<T>> getStaticItems() {
        return items.subList(0, staticCount);
    }

    protected List<ItemInfo<T>> getDynamicItems() {
        return items.subList(staticCount, items.size());
    }

    private double getMinDistance(List<ItemInfo<T>> list, ItemInfo<T> item) {
        double min_dist = Double.MAX_VALUE;
        for (ItemInfo<T> existing : list) {
            double dist = existing.getItem().getLocation()
                    .distance(item.getItem().getLocation());
            if (dist < min_dist
                    && (Double.isNaN(existing.getDistance()) || dist < existing
                            .getDistance())) {
                min_dist = dist;
            }
        }
        return min_dist;

    }

    public static interface PlotItem {

        public Coordinate getLocation();
    }

    private static class ItemInfo<T extends PlotItem> {

        private final T item;

        private double distance;

        public ItemInfo(T item) {
            this.item = item;
            this.distance = Double.NaN;
        }

        public T getItem() {
            return item;
        }

        public boolean within(IExtent extent) {
            Coordinate location = item.getLocation();
            return extent.contains(new double[] { location.x, location.y });
        }

        public boolean hasDistance() {
            return !Double.isNaN(distance);
        }

        public double getDistance() {
            return distance;
        }

        public void setDistance(double distance) {
            this.distance = distance;
        }

    }
}
