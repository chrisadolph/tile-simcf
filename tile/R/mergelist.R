"mergelist" <-
function(a,b) {
    if (is.null(a)) {
        c <- b
    } else {    
        if (is.list(a) & is.list(b)) {
            c <- a
            uabn <- union(names(a),names(b))
            if (length(uabn)!=length(names(a))) {
                for (i in (length(c)+1):(length(uabn))) {
                    c <- c(c,NA)
                    names(c)[i] <- uabn[i]
                    c[i] <- b[uabn[i]]
                }
            }
            for (i in (1:length(c))) {
                if (is.list(c[[names(c)[i]]])&&length(intersect(names(b),names(c)[i]))&&is.list(b[[names(c)[i]]])) {
                    c[[names(c)[i]]] <- mergelist(c[[names(c)[i]]],b[[names(c)[i]]])
                    # Note recursion
                }
            }
        } else {
            print("Error in mergelist:  Input not a list");
            stop()
        }
    }
    c
}

