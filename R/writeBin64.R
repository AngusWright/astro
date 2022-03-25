writeBin64 = function(object, con, size = NA_integer_, endian = .Platform$endian, useBytes = FALSE){
    swap = endian != .Platform$endian
    if(!is.vector(object) || mode(object)=="list"){
        stop("can only write vector objects")
    }
    if(is.character(con)){
        con = file(con, "wb")
        on.exit(close(con))
    }
    if((length(object)*size) > (2^31-1)){
        chunks = ceiling(length(object)*size/(2^30))
        excess = (length(object) %% (2^30))
        for(i in 1:(chunks-1)){
            writeBin(object=object[1:((2^30)/size)+(i-1)*((2^30)/size)], con=con, size=size, endian=endian, useBytes=useBytes)
        }
        writeBin(object=object[1:excess+(chunks-1)*((2^30)/size)], con=con, size=size, endian=endian, useBytes=useBytes)
    }else{
        writeBin(object=object, con=con, size=size, endian=endian, useBytes=useBytes)
    }
}

