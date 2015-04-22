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

import java.awt.Color;
import java.io.DataInput;
import java.io.IOException;

/**
 * Bitonal Tile.
 * Class=4, Element=28
 * @version $Id:  $ 
 * @author  xphc
 * @since Oct 5, 2010
 */
public class BitonalTile extends TileElement {
	private Color backgroundColor = null;
	private int backgroundColorIndex;
	private Color foregroundColor = null;
	private int foregroundColorIndex;

	//ORIGINAL//BitonalTile(int ec, int eid, int l, DataInput in) throws IOException {
	public BitonalTile(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);

		this.compressionType = CompressionType.get(makeIndex());
		this.rowPaddingIndicator = makeInt();

		if (ColourSelectionMode.getType().equals(ColourSelectionMode.Type.DIRECT)) {
			this.backgroundColor = makeDirectColor();
		}
		else if (ColourSelectionMode.getType().equals(ColourSelectionMode.Type.INDEXED)) {
			this.backgroundColorIndex = makeColorIndex();
		}

		if (ColourSelectionMode.getType().equals(ColourSelectionMode.Type.DIRECT)) {
			this.foregroundColor = makeDirectColor();
		}
		else if (ColourSelectionMode.getType().equals(ColourSelectionMode.Type.INDEXED)) {
			this.foregroundColorIndex = makeColorIndex();
		}

		readSdrAndBitStream();
	}

	@Override
	protected void readBitmap() {
		unsupported("BITMAP for BitonalTile");
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("BitonalTile [compressionType=");
		builder.append(this.compressionType);
		builder.append(", rowPaddingIndicator=");
		builder.append(this.rowPaddingIndicator);
		builder.append(", bufferedImage=");
		builder.append(this.bufferedImage);
		builder.append(", backgroundColor=");
		builder.append(this.backgroundColor);
		builder.append(", backgroundColorIndex=");
		builder.append(this.backgroundColorIndex);
		builder.append(", foregroundColor=");
		builder.append(this.foregroundColor);
		builder.append(", foregroundColorIndex=");
		builder.append(this.foregroundColorIndex);
		builder.append("]");
		return builder.toString();
	}

}
