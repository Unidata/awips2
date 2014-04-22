package com.raytheon.viz.grid.util;

import java.util.ArrayList;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.factory.Hints;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.ReferencingFactoryFinder;
import org.geotools.referencing.operation.AbstractCoordinateOperationFactory;
import org.geotools.referencing.operation.transform.IdentityTransform;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.GeographicCRS;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.CoordinateOperationFactory;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.MathTransformFactory;
import org.opengis.referencing.operation.TransformException;

// TODO: is .getEnvelope() correct?

/**
 * From AWIPS 1 GridPVDepict.C:
 * 
 * This method determines whether the mapping between gridded data and the
 * display space is severe enough to require the gridded data to be remapped
 * before display.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date          Ticket#   Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 19, 2012  14988    D. Friedman Initial revision
 * Sep 24, 2013  15972    D. Friedman Do not require contiguous mapping.
 * Jan 15, 2014  2661     bsteffen    Disable output
 * 
 * </pre>
 */
public class ConformalityUtil {

    private static final boolean DEBUG = false;

    public static boolean testConformality(GeneralGridGeometry sourceGG, GeneralGridGeometry destGG) {
        ConformalityUtil test = new ConformalityUtil(sourceGG, destGG);
        return test.testConformality();
    }
    
    GeneralGridGeometry sourceGG;

    GeneralGridGeometry destGG;

    double minRatio;

    double maxRatio;

    MathTransform lastMT;
    
    private ConformalityUtil(GeneralGridGeometry sourceGG, GeneralGridGeometry destGG) {
        this.sourceGG = sourceGG;
        this.destGG = destGG;
        
    }
    
    public boolean testConformality() {
        CoordinateReferenceSystem srcCRS = sourceGG.getCoordinateReferenceSystem();
        CoordinateReferenceSystem dstCRS = destGG.getCoordinateReferenceSystem();
        GeneralGridGeometry evaluatedDomain = null;
        
        if (srcCRS == null || dstCRS == null)
            return false;
        if (CRS.equalsIgnoreMetadata(srcCRS, dstCRS))
            return true;
        
        boolean evaluated = false;
        
        resetRatios();
        evaluated = evaluateEnvelope(destGG, sourceGG);
        if (evaluated) {
            evaluatedDomain = destGG;
        } else {
            resetRatios();
            evaluated = evaluateEnvelope(sourceGG, destGG);
            evaluatedDomain = sourceGG;
        }
        
        double ar = evaluatedDomain.getEnvelope().getSpan(0)/
            evaluatedDomain.getEnvelope().getSpan(1);
        if (ar < 1)
            ar = 1 / ar;
        final double maxRatioRatio = 2.0;   
        double rr = maxRatio / minRatio;
        
        if (! evaluated || maxRatio / minRatio > maxRatioRatio) {
            if (DEBUG) {
                System.out.format("%s -> %s : not conformal enough (%f)\n",
                        sourceGG, destGG, maxRatio / minRatio);
            }
            return false;
        }
        
        resetRatios();
        
        if (! evaluateNonContig(evaluatedDomain)) {
            if (DEBUG) {
                System.out.format("%s -> %s : not contiguous?\n", sourceGG,
                        destGG);
            }
            return false;
        /*
         * This test is not necessary for AWIPS II because it can cope
         * with non-contiguous mappings.
         */
        /*
        } else if (maxRatio/minRatio > maxRatioRatio ||
                (minRatio > 0 && maxRatio/minRatio > maxRatioRatio) || // ?
                (minRatio < 0 && minRatio/maxRatio > maxRatioRatio)) {
            System.out.format("%s -> %s : not conformal enough somehow\n", sourceGG, destGG);
            return false;
        */
        } else {
            if (DEBUG) {
                System.out.format("%s -> %s : conformal enough (%f, %f)\n",
                        sourceGG, destGG, rr, maxRatio / minRatio);
            }
            return true;
        }
    }
    
    private boolean evaluateNonContig(GeneralGridGeometry evaluatedDomain) {
        int nEval = 0;
        Envelope e = evaluatedDomain.getEnvelope();
        
        DirectPosition2D pivot = new DirectPosition2D(
            (e.getMinimum(0)+e.getMaximum(0)) / 2, 
            (e.getMinimum(1)+e.getMaximum(1)) / 2);
        if (ep(pivot, (e.getMinimum(0)+e.getMaximum(0)) / 2, e.getMinimum(1))) ++nEval;
        if (ep(pivot, (e.getMinimum(0)+e.getMaximum(0)) / 2, e.getMaximum(1))) ++nEval;
        if (ep(pivot, e.getMinimum(0), (e.getMinimum(1)+e.getMaximum(1)) / 2)) ++nEval;
        if (ep(pivot, e.getMaximum(0), (e.getMinimum(1)+e.getMaximum(1)) / 2)) ++nEval;
        if (ep(pivot, (pivot.x+e.getMaximum(0)*999999)/1000000,
                (pivot.y+e.getMaximum(1)*999999)/1000000)) ++nEval;
        if (ep(pivot, (pivot.x+e.getMinimum(0)*999999)/1000000,
                (pivot.y+e.getMaximum(1)*999999)/1000000)) ++nEval;
        if (ep(pivot, (pivot.x+e.getMaximum(0)*999999)/1000000,
                (pivot.y+e.getMinimum(1)*999999)/1000000)) ++nEval;
        if (ep(pivot, (pivot.x+e.getMinimum(0)*999999)/1000000,
                (pivot.y+e.getMinimum(1)*999999)/1000000)) ++nEval;
        
        return nEval >= 4;
    }

    private boolean ep(DirectPosition2D base, double x, double y) {
        DirectPosition2D point = new DirectPosition2D(x, y);
        DirectPosition2D br = new DirectPosition2D();
        DirectPosition2D pr = new DirectPosition2D();
        if (! xf(base, br) || ! xf(point, pr))
            return false;
        
        pr.x -= br.x;
        pr.y -= br.y;
        point.x -= base.x;
        point.y -= base.y;

        //double conf = (pr.x*point.x+pr.y*point.y)/Math.sqrt(pr.x*pr.x+pr.y*pr.y);//?
        double conf = Math.sqrt(point.x*point.x+point.y*point.y)/Math.sqrt(pr.x*pr.x+pr.y*pr.y);
        if (conf<minRatio) minRatio = conf;
        if (conf>maxRatio) maxRatio = conf;

        return true;
    }

    private boolean evaluateEnvelope(GeneralGridGeometry aGG, GeneralGridGeometry bGG) {
        CoordinateReferenceSystem aCRS = aGG.getCoordinateReferenceSystem();
        CoordinateReferenceSystem bCRS = bGG.getCoordinateReferenceSystem();
        GeographicCRS aGeoCRS = null;
        GeographicCRS bGeoCRS = null;
        ArrayList<MathTransform> transforms = new ArrayList<MathTransform>();
        
        if (aCRS instanceof ProjectedCRS) {
            aGeoCRS = ((ProjectedCRS) aCRS).getBaseCRS();
        }
        if (bCRS instanceof ProjectedCRS) {
            bGeoCRS = ((ProjectedCRS) bCRS).getBaseCRS();
        }
        try {
            transforms.add(CRS.findMathTransform(bCRS, bGeoCRS, true));
            if (CRS.equalsIgnoreMetadata(aGeoCRS, bGeoCRS)) {
                // nothing...
            } else {
                transforms.add(CRS.findMathTransform(bGeoCRS, aGeoCRS));
            }
            transforms.add(CRS.findMathTransform(aGeoCRS, aGG.getCoordinateReferenceSystem(), true));
        } catch (FactoryException e) {
            //statusHandler.error(e.getMessage(), e);
            return false;
        }
        
        MathTransform mt;
        try {
            mt = concatenateTransforms(transforms);
        } catch (FactoryException e) {
            //statusHandler.error(e.getMessage(), e);
            return false;
        }
        
        lastMT = mt;
        
        Envelope e = aGG.getEnvelope();
        return (evaluatePoint((e.getMinimum(0)+e.getMaximum(0)) / 2, 
                    (e.getMinimum(1)+e.getMaximum(1)) / 2) && 
                evaluatePoint(e.getMinimum(0), e.getMinimum(1)) && 
                evaluatePoint(e.getMinimum(0), e.getMaximum(1)) && 
                evaluatePoint(e.getMaximum(0), e.getMaximum(1)) && 
                evaluatePoint(e.getMaximum(0), e.getMinimum(1)));
    }
    
    private boolean evaluatePoint(double x, double y) {
        DirectPosition2D point = new DirectPosition2D(x, y);
        DirectPosition2D remap = new DirectPosition2D();
        DirectPosition2D offset = new DirectPosition2D();
        
        if (! xf(point, remap))
            return false;
        
        point.x += 0.0001;
        if (! xf(point, offset)) {
            point.x = x - 0.0001;
            if (! xf(point, offset))
                return false;
        }

        offset.x -= remap.x;
        offset.y -= remap.y;
        double ratio = Math.sqrt(offset.x*offset.x+offset.y*offset.y)/0.0001;
        if (ratio<minRatio) minRatio = ratio;
        if (ratio>maxRatio) maxRatio = ratio;

        point.x = x;
        point.y += 0.0001;
        if (! xf(point, offset)) {
            point.y = y - 0.0001;
            if (! xf(point, offset))
                return false;
        }

        offset.x -= remap.x;
        offset.y -= remap.y;
        ratio = Math.sqrt(offset.x*offset.x+offset.y*offset.y)/0.0001;
        if (ratio<minRatio) minRatio = ratio;
        if (ratio>maxRatio) maxRatio = ratio;
        
        return true;
    }
        
    private boolean xf(DirectPosition2D a, DirectPosition2D b) {
        try {
            lastMT.transform(a, b);
        } catch (TransformException e) {
            return false;
        }
        return true;
    }

    private static MathTransform concatenateTransforms(ArrayList<MathTransform> transforms) throws FactoryException {
        Hints hints = new Hints();
        final CoordinateOperationFactory factory =
            ReferencingFactoryFinder.getCoordinateOperationFactory(hints);
        final MathTransformFactory mtFactory;
        if (factory instanceof AbstractCoordinateOperationFactory) {
            mtFactory = ((AbstractCoordinateOperationFactory) factory).getMathTransformFactory();
        } else {
            mtFactory = ReferencingFactoryFinder.getMathTransformFactory(hints);
        }
        
        MathTransform mt = null;
        for (MathTransform mti : transforms) {
            if (mt == null)
                mt = mti;
            else {
                mt = mtFactory.createConcatenatedTransform(mt, mti);
            }
        }
        
        return mt != null ? mt : IdentityTransform.create(2);
    }
    
    private void resetRatios() {
        minRatio = Double.MAX_VALUE;
        maxRatio = -Double.MAX_VALUE;
    }
}
