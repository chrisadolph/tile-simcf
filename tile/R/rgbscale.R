"rgbscale" <-
function(s,r) {
    z <- 0*s;
    if ((r=="gray")||(r=="grey")||(r=="black"))  {
      collist <- rgb(.9*s,.9*s,.9*s);
    };
    if (r=="red")   {
      collist <- rgb(.8*s+0.2,z,z);
    };
    if (r=="green") {
      collist <- rgb(z,.8*s+0.2,z);
    };
    if (r=="blue")  {
      collist <- rgb(z,z,.8*s+0.2);
    };
    collist <- rev(collist);
    collist
}

