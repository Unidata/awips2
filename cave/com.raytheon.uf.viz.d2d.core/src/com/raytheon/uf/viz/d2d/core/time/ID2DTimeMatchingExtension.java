package com.raytheon.uf.viz.d2d.core.time;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * Allows a resource to modify time matching behavior
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2014-05-05   DR 17201   D. Friedman Initial revision.
 * 
 * </pre>
 * 
 */
public interface ID2DTimeMatchingExtension {
    public void modifyTimeMatching(D2DTimeMatcher d2dTimeMatcher, AbstractVizResource<?, ?> rsc, TimeMatcher timeMatcher);
}
