
var canvas = null;
var ctx = null;

function setup (){
    canvas = document.getElementById("myCanvas");
    // console.log("setup called " , canvas);
    ctx = canvas.getContext("2d");
    console.log("setup called " , canvas , ctx);
    canvas.addEventListener("mousemove", (event) => { mouseMoveOnCanvas(event) });
    canvas.addEventListener("mousedown", (event) => { mouseDownOnCanvas(event) });
    canvas.addEventListener("mouseup", (event) => { mouseUpOnCanvas(event) });
    line();
}

function getMousePos (canvas , evt){
    var rect = canvas.getBoundingClientRect();
    return {
	x: evt.clientX - rect.left,
	y: evt.clientY - rect.top
    };
}


function mouseMoveOnCanvas (evt){
    var pos = getMousePos(canvas, evt);
    ctx = canvas.getContext("2d");

    ctx.font = "30px Arial";
    //ctx.fillText("Hello World", pos.x , pos.y);
}

function mouseDownOnCanvas (evt){
    var pos = getMousePos(canvas, evt);
    ctx = canvas.getContext("2d");

    ctx.moveTo(pos.x-10, pos.y);
    ctx.lineTo(pos.x+10, pos.y);
    ctx.moveTo(pos.x, pos.y-10);
    ctx.lineTo(pos.x, pos.y+10);
    ctx.stroke();

    //ctx.font = "30px Arial";
    //ctx.fillText("Hello World", pos.x , pos.y);
}


function line (){
    //var c = document.getElementById("myCanvas");
    //var ctx = c.getContext("2d");
    
    ctx = canvas.getContext("2d");
    ctx.moveTo(0, 0);
    ctx.lineTo(200, 100);
    ctx.stroke();

    // circle
    ctx.beginPath();
    ctx.arc(95, 50, 40, 0, 2 * Math.PI);
    ctx.stroke();

    // text
    ctx.font = "30px Arial";
    ctx.fillText("Hello World", 10, 50);

    // stroke text
    ctx.font = "30px Arial";
    ctx.strokeText("Hello World", 10, 100);

    
}

