/*
 * Copyright (c) 2010, Swiss AviationSoftware Ltd. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - Neither the name of the Swiss AviationSoftware Ltd. nor the names of its
 *   contributors may be used to endorse or promote products derived from this
 *   software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm;

import java.io.DataInput;
import java.io.IOException;

/**
 * Class=2, Element=9
 * @version $Id:  $ 
 * @author  xphc
 * @since Oct 5, 2010
 */
public class DeviceViewportSpecificationMode extends Command {
	enum Mode {
		FractionOfDrawingSurface,
		MillimetersWithScaleFactor,
		PhysicalDeviceCoordinates
	}

	private static Mode specifier;
	private final double metricScaleFactor;

	static {
		reset();
	}

	//ORIGINAL//DeviceViewportSpecificationMode(int ec, int eid, int l, DataInput in) throws IOException {
	public DeviceViewportSpecificationMode(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);

		int e = makeEnum();
		switch (e) {
		case 0:
			specifier = Mode.FractionOfDrawingSurface;
			break;
		case 1:
			specifier = Mode.MillimetersWithScaleFactor;
			break;
		case 2:
			specifier = Mode.PhysicalDeviceCoordinates;
			break;
		default:
			unsupported("unsupported mode " + e);
		}

		if (RealPrecision.hasRealPrecisionBeenProcessed()) {
			this.metricScaleFactor = makeReal();
		}
		else {
			this.metricScaleFactor = makeFloatingPoint32();
		}

		// make sure all the arguments were read
		assert (this.currentArg == this.args.length);
	}

	private static void reset() {
		specifier = Mode.FractionOfDrawingSurface;
	}

	static Mode getMode() {
		return specifier;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("DeviceViewportSpecificationMode [specifier=");
		builder.append(specifier);
		builder.append(", metricScaleFactor=");
		builder.append(this.metricScaleFactor);
		builder.append("]");
		return builder.toString();
	}

}
