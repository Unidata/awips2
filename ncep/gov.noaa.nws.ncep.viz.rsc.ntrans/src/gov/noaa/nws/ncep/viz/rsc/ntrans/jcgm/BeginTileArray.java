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

import java.awt.geom.Point2D;
import java.awt.geom.Point2D.Double;
import java.io.DataInput;
import java.io.IOException;

/**
 * Tile Array.
 * Class=0, Element=19
 * @version $Id:  $ 
 * @author  xphc (Philippe Cadé)
 * @since Oct 5, 2010
 */
public class BeginTileArray extends Command {
	private final Double position;
	private final int cellPathDirection;
	private final int lineProgressionDirection;
	private final int nTilesInPathDirection;
	private final int nTilesInLineDirection;
	private final int nCellsPerTileInPathDirection;
	private final int nCellsPerTileInLineDirection;
	private final double cellSizeInPathDirection;
	private final double cellSizeInLineDirection;
	private final int imageOffsetInPathDirection;
	private final int imageOffsetInLineDirection;
	private final int nCellsInPathDirection;
	private final int nCellsInLineDirection;

	//ORIGINAL//BeginTileArray(int ec, int eid, int l, DataInput in) throws IOException {
	public BeginTileArray(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);

		this.position = makePoint();
		this.cellPathDirection = makeEnum();
		this.lineProgressionDirection = makeEnum();
		this.nTilesInPathDirection = makeInt();
		this.nTilesInLineDirection = makeInt();
		this.nCellsPerTileInPathDirection = makeInt();
		this.nCellsPerTileInLineDirection = makeInt();
		this.cellSizeInPathDirection = makeReal();
		this.cellSizeInLineDirection = makeReal();
		this.imageOffsetInPathDirection = makeInt();
		this.imageOffsetInLineDirection = makeInt();
		this.nCellsInPathDirection = makeInt();
		this.nCellsInLineDirection = makeInt();

		// make sure all the arguments were read
		assert (this.currentArg == this.args.length);
	}

	@Override
	public void paint(CGMDisplay d) {
		Point2D.Double startPosition = new Point2D.Double(this.position.x - this.imageOffsetInPathDirection * this.cellSizeInPathDirection,
				this.position.y + this.imageOffsetInLineDirection * this.cellSizeInLineDirection);

		// this is the bounding box for the whole tile array
		double boundingBoxSizeInPathDirection = this.nCellsInPathDirection / this.cellSizeInPathDirection;
		double boundingBoxSizeInLineDirection = this.nCellsInLineDirection / this.cellSizeInLineDirection;

		double tileSizeInPathDirection = boundingBoxSizeInPathDirection / this.nTilesInPathDirection;
		double tileSizeInLineDirection = boundingBoxSizeInLineDirection / this.nTilesInLineDirection;

		d.setTileArrayInfo(new TileArrayInfo(startPosition, this.nTilesInPathDirection, tileSizeInPathDirection, tileSizeInLineDirection));
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("BeginTileArray [position=");
		builder.append(this.position);
		builder.append(", cellPathDirection=");
		builder.append(this.cellPathDirection);
		builder.append(", lineProgressionDirection=");
		builder.append(this.lineProgressionDirection);
		builder.append(", nTilesInPathDirection=");
		builder.append(this.nTilesInPathDirection);
		builder.append(", nTilesInLineDirection=");
		builder.append(this.nTilesInLineDirection);
		builder.append(", nCellsPerTileInPathDirection=");
		builder.append(this.nCellsPerTileInPathDirection);
		builder.append(", nCellsPerTileInLineDirection=");
		builder.append(this.nCellsPerTileInLineDirection);
		builder.append(", cellSizeInPathDirection=");
		builder.append(this.cellSizeInPathDirection);
		builder.append(", cellSizeInLineDirection=");
		builder.append(this.cellSizeInLineDirection);
		builder.append(", imageOffsetInPathDirection=");
		builder.append(this.imageOffsetInPathDirection);
		builder.append(", imageOffsetInLineDirection=");
		builder.append(this.imageOffsetInLineDirection);
		builder.append(", nCellsInPathDirection=");
		builder.append(this.nCellsInPathDirection);
		builder.append(", nCellsInLineDirection=");
		builder.append(this.nCellsInLineDirection);
		builder.append("]");
		return builder.toString();
	}

}
