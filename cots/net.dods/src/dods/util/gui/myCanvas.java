

package dods.util.gui;
import java.awt.*;

class myCanvas extends Canvas {
	Image saved_i;

	public myCanvas(Image i, int h, int w){
		this.resize(h,w);
		saved_i = i;
	}

	public void paint(Graphics g){
		g.drawImage(saved_i,10,10,this);
	}
}


