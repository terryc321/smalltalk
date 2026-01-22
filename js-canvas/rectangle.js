

// rectangles
// how do we create a rectangle ?
//


function makeRectangle (x,y,wid,hgt){
    let rect = Object.create(null);
    // we never just refer to rect variables directly - always through procedure
    // gives us flexibility in changing internal structure of object variables - renaming etc
    // whilst keeping internal interface unchanged
    
    rect.fillStyleCold = function () { return "yellow" }
    rect.fillStyleHot = function () { return "red" }
    rect.x = function (){ return x }
    rect.y = function (){ return y }
    rect.wid = function (){ return wid }
    rect.hgt = function (){ return hgt }
    rect.lineWidth = function () { return 2 }
    
    rect.draw = function (){
	ctx.rect(rect.x() , rect.y() , rect.wid() , rect.hgt());
	ctx.stroke();
    }
    
    rect.mouseMove = function (event){
	var pos = getMousePos(canvas,event);
	var handled = false;
	var prevFillStyle = ctx.fillStyle;
	var prevLineWidth = ctx.lineWidth;
	
	// ctx.lineWidth = rect.lineWidth();
	// ctx.rect(rect.x(),rect.y(), rect.wid(),rect.hgt());
	// ctx.stroke();

	if (pos.x > rect.x() && pos.x < (rect.x() + rect.wid())){
	    if (pos.y > rect.y() && pos.y < (rect.y() + rect.hgt())){
		ctx.fillStyle = rect.fillStyleHot();
		ctx.fillRect(rect.x(),rect.y(), rect.wid(),rect.hgt());
		handled = true;
	    }
	}
	if (!handled){
	    ctx.fillStyle = rect.fillStyleCold();
	    ctx.fillRect(rect.x(),rect.y(), rect.wid(),rect.hgt());
	    handled = true;	
	}
	
	// restore state 
	ctx.fillStyle = prevFillStyle;
	ctx.lineWidth = prevLineWidth;
    }
    
    canvas.addEventListener("mousemove", (event) => { rect.mouseMove(event) });
    return rect;
}


function emptyRectangle (){
    ctx.rect(410,10, 150,100);
    ctx.stroke();
}

function fillRectangle (){
    ctx.fillStyle = "pink";
    ctx.fillRect(310,10, 150,100);
}


function rectangleMouseMove (event){
    var pos = getMousePos(canvas,event);
    var handled = false;
    var prevFillStyle = ctx.fillStyle;

    if (pos.x > 310 && pos.x < (310+150)){
	if (pos.y > 10 && pos.y < (10+100)){
	    ctx.fillStyle = "green";
	    ctx.fillRect(310,10, 150,100);
	    handled = true;
	}
    }
    if (!handled){
	ctx.fillStyle = "orange";
	ctx.fillRect(310,10, 150,100);
	handled = true;	
    }
    ctx.fillStyle = prevFillStyle;
}


