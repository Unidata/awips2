/*
 * <copyright> Copyright 1997-2003 BBNT Solutions, LLC under sponsorship of the
 * Defense Advanced Research Projects Agency (DARPA).
 * Copyright 2009 Swiss AviationSoftware Ltd.
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the Cougaar Open Source License as published by DARPA on
 * the Cougaar Open Source Website (www.cougaar.org).
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm;

import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.io.*;


/**
 * Class=4, Element=17
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class EllipseElement extends Command {
	protected Point2D.Double center;
	protected Point2D.Double firstConjugateDiameterEndPoint;
	protected Point2D.Double secondConjugateDiameterEndPoint;
	protected Shape ellipse;

    public EllipseElement(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
        
        this.center = makePoint();
        this.firstConjugateDiameterEndPoint = makePoint();
        this.secondConjugateDiameterEndPoint = makePoint();
    }

	/**
	 * Creates the ellipse on the first call. Returns the previously computed
	 * ellipse on subsequent calls.
	 * 
	 * @param d
	 */
	protected void initializeShape(CGMDisplay d) {
		if (this.ellipse != null) {
			// shape has been initialized already
			return;
		}

		double centerFirstDistance = distance(this.center, this.firstConjugateDiameterEndPoint);
		double centerSecondDistance = distance(this.center, this.secondConjugateDiameterEndPoint);

		double firstConjugateangle = Math.atan2(this.firstConjugateDiameterEndPoint.y -
												this.center.y,
			this.firstConjugateDiameterEndPoint.x - this.center.x);

		double secondFirstConjugateAngle = Math.atan2(this.secondConjugateDiameterEndPoint.y -
														this.center.y,
			this.secondConjugateDiameterEndPoint.x - this.center.x) -
											firstConjugateangle;

		centerSecondDistance = Math.abs(Math.sin(secondFirstConjugateAngle)) * centerSecondDistance;

		this.ellipse = createShape(centerFirstDistance, centerSecondDistance);

		AffineTransform translateInstance = AffineTransform.getTranslateInstance(this.center.x,
			this.center.y);

		AffineTransform rotationTransform = AffineTransform.getRotateInstance(firstConjugateangle);
		AffineTransform invertedRotationTransform = AffineTransform
				.getRotateInstance(-firstConjugateangle);

		Point2D.Double rotatedSecondConjugate = new Point2D.Double();
		invertedRotationTransform.transform(new Point2D.Double(
				this.secondConjugateDiameterEndPoint.x - this.center.x,
				this.secondConjugateDiameterEndPoint.y - this.center.y), rotatedSecondConjugate);

		AffineTransform shearTransform;
		if (rotatedSecondConjugate.y != 0) {
			shearTransform = AffineTransform.getShearInstance(rotatedSecondConjugate.x /
																rotatedSecondConjugate.y, 0);
		}
		else {
			// identity
			shearTransform = new AffineTransform();
		}

		// first, apply the shear, then the rotation and finally the translation
		rotationTransform.concatenate(shearTransform);

		try {
			applyArcs(d, rotationTransform.createInverse());
		}
		catch (NoninvertibleTransformException e) {
			// the matrix cannot be inverted, don't apply the arcs then
			info("cannot invert matrix");
		}

		translateInstance.concatenate(rotationTransform);

		GeneralPath generalPath = new GeneralPath(this.ellipse);
		this.ellipse = generalPath.createTransformedShape(translateInstance);
	}

	/**
	 * Defines the arc for this ellipse, if any
	 * @param d
	 * @param transform The inverted transformation that is applied to the ellipse 
	 */
	protected void applyArcs(CGMDisplay d, AffineTransform transform) {
		// no arcs here, only used in sub classes
	}

	@Override
	public void paint(CGMDisplay d) {
		initializeShape(d);
		
		Graphics2D g2d = d.getGraphics2D();
		
		if (this.ellipse == null)
			return;
		
		d.fill(this.ellipse);
		
		if (d.drawEdge()) {
			g2d.setColor(d.getEdgeColor());
			g2d.setStroke(d.getEdgeStroke());
			g2d.draw(this.ellipse);
		}
	}
	
	/**
	 * Returns the distance between the points p1 and p2
	 * @param p1
	 * @param p2
	 * @return
	 */
	protected double distance(Point2D.Double p1, Point2D.Double p2) {
		return Math.hypot(p2.x-p1.x, p2.y-p1.y);

	}

	/**
	 * Creates the ellipse around the origin.
	 * @param centerFirstDistance Width of the ellipse
	 * @param centerSecondDistance Height of the ellipse
	 * @return
	 */
	protected Shape createShape(double centerFirstDistance, double centerSecondDistance) {
		return new Ellipse2D.Double(-centerFirstDistance ,
				-centerSecondDistance , 2 * centerFirstDistance,
				2 * centerSecondDistance);
	}

	@Override
	public String toString() {
        return "Ellipse [" + this.center.x + "," + this.center.y + "] [" +
				this.firstConjugateDiameterEndPoint.x + "," +
				this.firstConjugateDiameterEndPoint.y + "] [" +
				this.secondConjugateDiameterEndPoint.x + "," +
				this.secondConjugateDiameterEndPoint.y + "]";
    }
}

/*
 * vim:encoding=utf8
 */
