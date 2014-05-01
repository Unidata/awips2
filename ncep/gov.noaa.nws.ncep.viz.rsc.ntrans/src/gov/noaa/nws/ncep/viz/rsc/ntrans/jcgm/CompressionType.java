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

/**
 * Compression types. Source: <a
 * href="http://jitc.fhu.disa.mil/nitf/graph_reg/class_pages/compression.html"
 * >INTERNATIONAL REGISTER OF ITEMS - Compression Type Section</a>
 * 
 * @version $Id: $
 * @author xphc
 * @since Oct 5, 2010
 */
public enum CompressionType {
	NULL_BACKGROUND(0),
	NULL_FOREGROUND(1),
	T6(2),
	/** T4 1-dimension */
	T4_1(3),
	/** T4 2-dimension */
	T4_2(4),
	/** Bitmap (uncompressed) */
	BITMAP(5),
	RUN_LENGTH(6),
	BASELINE_JPEG(7, "jpeg"),
	LZW(8),
	/** PNG Compression Method 0 */
	PNG(9, "png")
	;

	private final int identifier;
	private final String formatName;

	CompressionType(int identifier) {
		this(identifier, null);
	}

	/**
	 * Creates a type
	 * 
	 * @param identifier
	 *            The identifier for the type
	 * @param formatName
	 *            The format name, used to be able to retrieve an ImageIO reader
	 */
	CompressionType(int identifier, String formatName) {
		this.identifier = identifier;
		this.formatName = formatName;
	}

	/**
	 * Returns the compression type for the given identifier.
	 * 
	 * @param identifier
	 *            The identifier to match
	 * @return The type or {@code null} if not found
	 */
	static CompressionType get(int identifier) {
		for (CompressionType type: values()) {
			if (type.identifier == identifier)
				return type;
		}
		return null;
	}

	String getFormatName() {
		return this.formatName;
	}
}
