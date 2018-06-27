#' flatXML: Tools for Working with XML Files as R Dataframes
#'
#' @section Overview:
#' \code{flatxml} provides functions to easily deal with XML files. When parsing an XML document with \code{\link{fxml_importXMLFlat}}, \code{flatxml} produces a special dataframe that is \'flat\' by its very nature but contains all necessary information about the hierarchical structure of the underlying XML document (for details on the dataframe see the reference for the \code{\link{fxml_importXMLFlat}} function).
#' \code{flatxml} offers a set of functions to work with this dataframe.
#' Apart from representing the XML document in a dataframe structure, there is yet another way in which \code{flatxml} relates to dataframes: the \code{\link{fxml_toDataFrame}} function can be used to extract data from an XML document into a dataframe, e.g. to work on the data with statistical functions. Because in this case there is no need to represent the XML document structure as such (it's all about the data contained in the document), there is no representation of the hierarchical structure of the document any more, it's just a normal dataframe.
#' Each XML element, for example \code{<tag attribute="some value">Here is some text</tag>} has certain characteristics that can be accessed via the \code{flatxml} interface functions, after an XML document has been imported with \code{\link{fxml_importXMLFlat}}. These characteristics are:
#' \itemize{
#' \item \emph{value}: The (text) value of the element, \code{"Here is some text"} in the example above
#' \item \emph{attributes}: The XML attributes of the element, \code{attribute} with its value \code{"some value"} in the example above
#' \item \emph{children}: The elements on the next lower hierarchical level
#' \item \emph{parent}: The element of the next higher hierarchical level, i.e. the element to which the current element is a child
#' \item \emph{siblings}: The elements on the same hierarchical level as the current element
#' }
#'
#'
#' @section Structure of the flatxml interface:
#' The \code{flatxml} interface to access these characteristics follows a simple logic: For each of the characteristics there are typically three functions available:
#' \itemize{
#' \item \code{fxml_has...()}: Determines if the current XML element has (at least one instance of) the characteristic
#' \item \code{fxml_num...()}: Returns the number of the characteristics of the current XML (e.g. the number of children elements)
#' \item \code{fxml_get...()}: Returns (the IDs of) the respective characteristics of the current XML element (e.g. the children of the current element)
#' }
#'
#'
#' @section Functions to access the characteristics of an XML element:
#'
#' For values:
#' \itemize{
#' \item \code{\link{fxml_hasValue}}
#' \item \code{\link{fxml_getValue}}
#' }
#' For attributes:
#' \itemize{
#' \item \code{\link{fxml_hasAttributes}}
#' \item \code{\link{fxml_numAttributes}}
#' \item \code{\link{fxml_getAttribute}} (note: no plural 's'!)
#' \item \code{\link{fxml_getAttributesAll}} (get all attributes instead of a specific one)
#' }
#' For children:
#' \itemize{
#' \item \code{\link{fxml_hasChildren}}
#' \item \code{\link{fxml_numChildren}}
#' \item \code{\link{fxml_getChildren}}
#' }
#' For parents:
#' \itemize{
#' \item \code{\link{fxml_hasParent}}
#' \item \code{\link{fxml_getParent}}
#' }
#' For siblings:
#' \itemize{
#' \item \code{\link{fxml_hasSiblings}}
#' \item \code{\link{fxml_numSiblings}}
#' \item \code{\link{fxml_getSiblings}}
#' }
#'
#'
#' @section Functions for searching in the XML document:
#' \itemize{
#' \item \code{\link{fxml_findPath}} (search anywhere in the path to an XML element)
#' \item \code{\link{fxml_findPathFull}} (find an element based on its complete path)
#' \item \code{\link{fxml_findPathRoot}} (search in the path to an XML element starting at the top element [root node])
#' \item \code{\link{fxml_findPathBottom}} (search in the path to an XML element starting at the lowest hierarchical level)
#' }
#'
#'
#' @section Other functions:
#'
#' \itemize{
#' \item \code{\link{fxml_getElement}} (name on an XML element (the \code{tag} in \code{<tag>…</tag>})
#' \item \code{\link{fxml_getUniqueElements}} (unique XML elements in the document)
#' \item \code{\link{fxml_getElementInfo}} (all relevant information on an XML element (children, siblings, etc.)
#' \item \code{\link{fxml_getDepthLevel}} (level of an element in the hierarchy of the XML document)
#' }
#' @docType package
#' @name flatxml
NULL


#' @title Handling flat XML files
#' @description Reads an XML document into a flat dataframe structure.
#'
#' @param path Path to the XML document. Can be either a local path or a URL.
#'
#' @return A dataframe containing the XML document in a flat structure. See the Details section for more information on its structure.
#'
#' @details The XML document is parsed and stored in a dataframe structure (flat XML).
#' The first four columns of a flat XML dataframe are standard columns. Their names all end with a dot. These columns are:
#' \itemize{
#' \item \code{elem.}: The element identifier of the current XML element (without the tag delimiters \code{<} and \code{>}).
#' \item \code{elemid.}: A unique, ascending numerical ID for each XML element. The first XML element is assigned 1 as its ID. This ID is used by many of the \code{flatxml} functions.
#' \item \code{attr.}: Name of an attribute. For each attribute of an XML element the dataframe will have an additional row.
#' \item \code{value.}: The value of either the attribute (if \code{attr.} is not \code{NA}) or the element itself (if \code{attr.} is \code{NA}). \code{value.} is \code{NA}, if the element has no value.
#' }
#' The columns after these four standard columns represent the 'path' to the current element, starting from the root element of the XML document in column 5 all
#' the way down to the current element. The number of columns of the dataframe is therefore determined by the depth of the hierarchical structure of the XML document.
#' In this dataframe representation, the hierarchical structure of the XML document becomes very easy to understand. All \code{flatxml} functions work with this flat XML dataframe.
#'
#' If an XML element has N attributes it is represented by (N+1) rows in the flat XML dataframe: one row for the value (with \code{dataframe$value.} being \code{NA} if the element has no value)
#' and one for each attribute. In the attribute rows, the names of the attributes are stored in the \code{attr.} field, their respecitive values in the \code{value.} field. Even if there are multiple rows
#' for one XML element, the \code{elem.} and \code{elemid.} fields still have the same value in all rows (because the rows belong to the same XML element).
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' example <- system.file("worldpopulation.xml", package="flatxml")

#' # Create flat dataframe from XML
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' @export
fxml_importXMLFlat <- function(path) {
  if(file.exists(path) | RCurl::url.exists(path)) xml<-xml2::read_xml(path)
  else stop(paste0("XML document '", path, "' cannot be found."))
  path<-character(0)
  df<-data.frame(elem.=character(0), elemid.=integer(0), attr.=character(0), value.=character(0), stringsAsFactors=FALSE)
  root <- xml2::xml_root(xml)
  return(flattenXML(root, df, path))
}



flattenXML<-function(node, frame, path) {
  fixed.cols <- 4
  elem. <- xml2::xml_name(node)
  if(nrow(frame) > 0) elemid. <- max(frame$elemid.) + 1
  else elemid. <- 1
  attr. <- NA
  children <- xml2::xml_children(node)

  txt<-xml2::xml_find_first(node, "./text()")
  if(length(txt)>0) value. <- as.character(txt)
  else value. <- NA

  path <- append(path, elem.)

  if(length(path) + fixed.cols > ncol(frame)) {
    x. <- rep(NA, nrow(frame))
    frame <- cbind(frame, x.)
    names(frame)[ncol(frame)] <- paste0("level", as.character(ncol(frame)-fixed.cols))
  }
  if(nrow(frame) > 0) {
    df <- frame[nrow(frame),]
    df$elem.[1] <- elem.
    df$elemid.[1] <- elemid.
    df$attr.[1] <- attr.
    df$value.[1] <- value.
    for(f in (fixed.cols+1):ncol(frame)) df[,f] <- NA
    if(length(path) > 0) for(f in 1:length(path)) df[, fixed.cols+f] <- path[f]
  }
  else {
    df <- data.frame(elem., elemid., attr., value., level1 = path[1], stringsAsFactors=FALSE)
  }

  frame <- rbind(frame, df)
  attr.list <- xml2::xml_attrs(node)
  if(is.character(attr.list)) {
    if(NROW(attr.list) > 0) {
      for(f in 1:NROW(attr.list)) {
        if(!is.na(names(attr.list)[f])) df$attr. <- names(attr.list)[f]
        else df$attr. <- ""
        if(!is.na(attr.list[f])) df$value. <- attr.list[f]
        else df$value. <- NA
        frame <- rbind(frame, df)
      }
    }
  }
  else {
    if(is.list(attr.list)) {
      if(NROW(attr.list[[1]]) > 0) {
        for(f in 1:NROW(attr.list[[1]])) {
          if(!is.na(names(attr.list[[1]])[f])) df$attr. <- names(attr.list[[1]])[f]
          else df$attr. <- ""
          if(!is.na(attr.list[[1]][f])) df$value. <- attr.list[[1]][f]
          else df$value. <- NA
          frame <- rbind(frame, df)
        }
      }
    }
  }

  if(length(children)>0) {
    for(f in 1:length(children)) {
      if(regexpr("<", as.character(children[f])) != -1) frame <- flattenXML(children[f], frame, path)
    }
  }
  rownames(frame) <- c()
  return(frame)
}



isFlatXML <- function(xmlflat.df) {
  res <- FALSE
  if(class(xmlflat.df)[1]=="data.frame") {
    if(ncol(xmlflat.df)[1] >= 4) {
      if(names(xmlflat.df)[1] == "elem." && names(xmlflat.df)[2] == "elemid." && names(xmlflat.df)[3] == "attr." && names(xmlflat.df)[4] == "value.") res <- TRUE
    }
  }
  return(res)
}



#' @title Handling flat XML files
#' @description Hierarchical position of an XML element
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return The number of the hierarchy level of the XML element with ID \code{elemid}. The root node of the XML data has hierarchy level 1.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Determine hierarchy level of XML element with ID 3 (xml.dataframe$elemid. ==  3)
#' fxml_getDepthLevel(xml.dataframe, 3)
#'
#' @export
fxml_getDepthLevel <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    res <- 0
    if(elemid %in% xmlflat.df$elemid.) {
      for(i in 5:ncol(xmlflat.df)) {
        if(is.na(xmlflat.df[xmlflat.df$elemid.==elemid, i])[1]) {
          res <- i-4-1
          break
        }
      }
      if(res == 0) res <- ncol(xmlflat.df)-4
    }
    return(res)
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Value of an XML element
#' @description Determines if an XML element carries a value.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return \code{TRUE} if the XML element has a value (not being equal to \code{NA}), \code{FALSE} otherwise.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_getValue}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Check if element with ID 4 (xml.dataframe$elemid. ==  4) carries a value
#' fxml_hasValue(xml.dataframe, 4)
#'
#' @export
fxml_hasValue <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) return(!is.na(xmlflat.df$value.[is.na(xmlflat.df$attr.) & xmlflat.df$elemid.==elemid]))
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Value of an XML element
#' @description Returns the value of an XML element.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return The value of the XML element with ID \code{elemid}. \code{NA} is returned if the element has no value.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_hasValue}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Get the value of the XML element with ID 4 (xml.dataframe$elemid. ==  4)
#' fxml_hasValue(xml.dataframe, 4)
#'
#' @export
fxml_getValue <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) return(xmlflat.df$value.[is.na(xmlflat.df$attr.) & xmlflat.df$elemid.==elemid])
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Attributes of XML elements
#' @description Determines if an XML element has any attributes.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return \code{TRUE} if the the XML element with ID \code{elemid} has at least one attribute, \code{FALSE} otherwise.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_getAttribute}}, \code{\link{fxml_numAttributes}}, \code{\link{fxml_getAttributesAll}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Check if the XML element with ID 4 (xml.dataframe$elemid. ==  4) has any attributes
#' fxml_hasAttributes(xml.dataframe, 4)
#'
#' @export
fxml_hasAttributes <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) return(fxml_numAttributes(xmlflat.df, elemid) > 0)
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Attributes of XML elements
#' @description Determines the number of attributes of an XML element.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return The number of attributes of the XML element with ID \code{elemid}.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_hasAttributes}}, \code{\link{fxml_getAttribute}}, \code{\link{fxml_getAttributesAll}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Determine the number of attributes of the XML element with ID 4 (xml.dataframe$elemid. ==  4)
#' fxml_numAttributes(xml.dataframe, 4)
#'
#' @export
fxml_numAttributes <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) return(sum((!is.na(xmlflat.df$attr.[xmlflat.df$elemid.==elemid]))))
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Attributes of an XML element
#' @description Returns the value of a specific attribute of an XML element.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#' @param attrib.name Name of the attribute.
#'
#' @return The value of attribute \code{attrib.name} of the XML element with ID \code{elemid}. If the attribute is not existing, an error message is shown.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_hasAttributes}}, \code{\link{fxml_numAttributes}}, \code{\link{fxml_getAttributesAll}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Read the value of attribute "name" from the XML element with ID 4 (xml.dataframe$elemid. ==  4)
#' fxml_getAttribute(xml.dataframe, 4, "name")
#' @export
fxml_getAttribute <- function(xmlflat.df, elemid, attrib.name) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) {
      if(attrib.name %in% xmlflat.df$attr.[xmlflat.df$elemid.==elemid]) {
        return(xmlflat.df$value.[xmlflat.df$elemid.==elemid & !is.na(xmlflat.df$attr.) & xmlflat.df$attr.==attrib.name][1])
      }
      else stop(paste0("No attribute with name '", attrib.name, "' available."))
    }
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Attributes of an XML element
#' @description Returns all attributes of an XML element and their respective values.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return A named vector containing the attribute values of all attributes of the XML element with ID \code{elemid}. The names of the vector are the names of the attributes. Returns \code{NULL} if the element has no attributes at all.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_hasAttributes}}, \code{\link{fxml_numAttributes}}, \code{\link{fxml_getAttribute}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Get all attribute of the XML element with ID 4 (xml.dataframe$elemid. ==  4)
#' fxml_getAttributesAll(xml.dataframe, 4)
#' @export
fxml_getAttributesAll <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) {
      res <- c()
      a <- xmlflat.df$attr.[xmlflat.df$elemid. == elemid & !is.na(xmlflat.df$attr.)]
      if(length(a) > 0) {
        for(i in 1:length(a)) {
          res[i] <- fxml_getAttribute(xmlflat.df, elemid, a[i])
        }
        names(res)<-a
      }
      return(res)
    }
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Children of an XML element
#' @description Determines if an XML element has any children.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return \code{TRUE}, if the the XML element with ID \code{elemid} has at least one child, \code{FALSE} otherwise.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_numChildren}}, \code{\link{fxml_getChildren}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Check, if the XML element with ID 4 (xml.dataframe$elemid. ==  4) has any
#' # children (sub-elements)
#' fxml_hasChildren(xml.dataframe, 4)
#' @export
fxml_hasChildren <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) return(fxml_getChildren(xmlflat.df, elemid)[1] > 0)
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Children of an XML element
#' @description Returns the children of an XML element.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return The IDs (\code{xmlflat.df$elemid.}) of the children of the XML element with ID \code{elemid}. If no children exist, \code{NULL} is returned.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_hasChildren}}, \code{\link{fxml_numChildren}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Get all the children (sub-elements) of the XML element with ID 4 (xml.dataframe$elemid. ==  4)
#' fxml_hasChildren(xml.dataframe, 4)
#' @export
fxml_getChildren <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) {
      res <- c()
      df<-xmlflat.df[xmlflat.df$elemid.>elemid & is.na(xmlflat.df$attr.),]
      if(nrow(df)>0) {
        for(i in 1:nrow(df)) {
          if(fxml_getDepthLevel(df, df$elemid.[i]) <= fxml_getDepthLevel(xmlflat.df, elemid)) break
          if(fxml_getDepthLevel(df, df$elemid.[i]) == fxml_getDepthLevel(xmlflat.df, elemid) + 1) res <- append(res, df$elemid.[i])
        }
      }
      return(res)
    }
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Children of an XML element
#' @description Determines the number of children of an XML element.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return The number of children of the XML element with ID \code{elemid}.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_hasChildren}}, \code{\link{fxml_getChildren}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Determine the number of children (sub-elements) of the XML element with ID 4
#' # (xml.dataframe$elemid. ==  4)
#' fxml_numChildren(xml.dataframe, 4)
#' @export
fxml_numChildren <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) {
      res <- fxml_getChildren(xmlflat.df, elemid)
      return(length(res))
    }
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}


#' @title Parent of an XML element
#' @description Returns the parent of an XML element.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return The ID (\code{xmlflat.df$elemid.}) of the parent node of the XML element with ID \code{elemid}. If no parent exists (because XML node \code{elemid} is the root node of the XML document) then \code{NULL} is returned.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_hasParent}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Get the ID of the parent element of the XML element with ID 4 (xml.dataframe$elemid. ==  4)
#' fxml_getParent(xml.dataframe, 4)
#' @export
fxml_getParent <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) {
      res<-NULL
      for(i in elemid:1) {
        if(fxml_getDepthLevel(xmlflat.df, i) == fxml_getDepthLevel(xmlflat.df, elemid) - 1) {
          res <- i
          break
        }
      }
      return(res)
    }
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Parent of an XML element
#' @description Determines, if an XML element has a parent element.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return \code{TRUE}, if a parent element for the XML element with ID \code{elemid} exists, \code{FALSE} otherwise (which would mean that the XML element is the root node of the XML document).
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_getParent}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Check if the XML element with ID 4 (xml.dataframe$elemid. ==  4) has a parent element
#' fxml_hasParent(xml.dataframe, 4)
#' @export
fxml_hasParent <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) {
      return(fxml_getParent(xmlflat.df, elemid) != 0)
    }
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Siblings of an XML element
#' @description Returns the siblings of an XML element, i.e. the elements on the same hierarchical level.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return The IDs (\code{xmlflat.df$elemid.}) of the siblings of the XML element with ID \code{elemid}. If no siblings exist, \code{NULL} is returned.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_hasSiblings}}, \code{\link{fxml_getSiblings}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Get all the siblings (elements on the same hierarchy level) of the XML element with ID 4
#' # (xml.dataframe$elemid. ==  4)
#' fxml_getSiblings(xml.dataframe, 4)
#' @export
fxml_getSiblings <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) {
      res<-NULL
      if(elemid != 1) {
        for(i in (elemid-1):1) {
          if(fxml_getDepthLevel(xmlflat.df, i) < fxml_getDepthLevel(xmlflat.df, elemid)) break
          if(fxml_getDepthLevel(xmlflat.df, i) == fxml_getDepthLevel(xmlflat.df, elemid)) res <- append(res, i)
        }
      }
      if(elemid != max(xmlflat.df$elemid.)) {
        for(i in (elemid+1):max(xmlflat.df$elemid.)) {
          if(fxml_getDepthLevel(xmlflat.df, i) < fxml_getDepthLevel(xmlflat.df, elemid)) break
          if(fxml_getDepthLevel(xmlflat.df, i) == fxml_getDepthLevel(xmlflat.df, elemid)) res <- append(res, i)
        }
      }
      return(res)
    }
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Siblings of an XML element
#' @description Determines if an XML element has any siblings, i.e. elements on the same hierarchical level.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return \code{TRUE}, if the the XML element with ID \code{elemid} has at least one sibling, \code{FALSE} otherwise.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#'#' @seealso \code{\link{fxml_numSiblings}}, \code{\link{fxml_getSiblings}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Check if XML element with ID 4 (xml.dataframe$elemid. ==  4) has any siblings
#' # (elements on the same hierarchy level)
#' fxml_hasSiblings(xml.dataframe, 4)
#' @export
fxml_hasSiblings <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) {
      return(!is.null(fxml_getSiblings(xmlflat.df, elemid)))
    }
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Siblings of an XML element
#' @description Determines the number of siblings of an XML element, i.e. elements on the same hierarchical level.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return The number of siblings of the XML element with ID \code{elemid}.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#'#' @seealso \code{\link{fxml_hasSiblings}}, \code{\link{fxml_getSiblings}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Determine the number of siblings (elements on the same hierarchy level) of the XML element
#' # with ID 4 (xml.dataframe$elemid. ==  4)
#' fxml_numSiblings(xml.dataframe, 4)
#' @export
fxml_numSiblings <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) {
      return(length(fxml_getSiblings(xmlflat.df, elemid)))
    }
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}


findPath <- function(xmlflat.df, path, attr.only = NULL, attr.not = NULL, mode="any") {
  if(isFlatXML(xmlflat.df)) {
    run.from <- 5
    run.to <- (ncol(xmlflat.df)-length(path)+1)
    if(mode=="root" || mode=="full") run.to <- 5
    res <- c()
    path.cat <- paste0("<", path, "<", collapse = "<")
    for(i in 1:nrow(xmlflat.df)) {
      if(mode == "bottom") {
        run.from <- 4 + fxml_getDepthLevel(xmlflat.df, xmlflat.df$elemid.[i]) - length(path) + 1
        run.to <- run.from
      }
      for(f in run.from:run.to) {
        if(is.na(xmlflat.df[i, "attr."])) {
          if(mode == "full") path.df <- paste0("<", xmlflat.df[i,5:(4 + fxml_getDepthLevel(xmlflat.df, xmlflat.df$elemid.[i]))], "<", collapse = "<")
          else path.df <- paste0("<", xmlflat.df[i,f:(f+length(path)-1)], "<", collapse = "<")
          if(path.df == path.cat) {
            is.match <- TRUE
            if(is.list(attr.only)) {
              for(z in 1:length(attr.only)) {
                if(names(attr.only)[z] %in% path) {
                  for(n in 1:length(attr.only[[z]])) {
                    for(x in i:1) {
                      if(xmlflat.df$elem.[x] == names(attr.only)[z]) {
                        if(fxml_getAttribute(xmlflat.df, xmlflat.df$elemid.[x], names(attr.only[[z]][n])) != attr.only[[z]][n]) is.match <- FALSE
                        break
                      }
                    }
                  }
                }
                else stop(paste0("XML tag '", names(attr.only)[z]), "' is not part of the search path.")
              }
            }
            if(is.list(attr.not)) {
              for(z in 1:length(attr.not)) {
                if(names(attr.not)[z] %in% path) {
                  for(n in 1:length(attr.not[[z]])) {
                    for(x in i:1) {
                      if(xmlflat.df$elem.[x] == names(attr.not)[z]) {
                        if(fxml_getAttribute(xmlflat.df, xmlflat.df$elemid.[x], names(attr.not[[z]][n])) == attr.not[[z]][n]) is.match <- FALSE
                        break
                      }
                    }
                  }
                }
                else stop(paste0("XML tag '", names(attr.only)[z]), "' is not part of the search path.")
              }
            }

            if(is.match == TRUE) res <- append(res, xmlflat.df[i, "elemid."])
          }
        }
      }
    }
    return(res)
  }
}



#' @title Finding XML elements
#' @description Finds all XML elements in an XML document that lie on a certain path, regardless of where exactly the path is found in the XML document. Sub-elements (children) of the elements on the search path are returned, too.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param path A character vector representing the path to be searched. Each element of the vector is a hierarchy level in the XML document. Example: \code{path = c("tag1", "tag2")}.
#' @param attr.only A list of named vectors representing attribute/value combinations the XML elements on the search path must match.
#' The name of an element in the list is the XML elment name to which the attribute belongs. The list element itself is a named vector.
#' The vector's elements represent different attributes (= the names of the vector elements) and their values (= vector elements).
#' Example: \code{attr.only = list(tag1 = c(attrib1 = "Value 1", attrib2 = "Value 2"), tag2 = c(attrib3 = "Value 3"))} will only find those elements which lie on a
#' path that includes \code{<tag1 attrib1 = "Value 1" attrib2 = "Value 2"><tag2 attrib3 = "Value 3">}.
#' @param attr.not A list of vectors representing attribute/value combinations the XML elements on the search path must not match to be included in the results. See argument \code{attr.only} for details on the composition.
#'
#' @return The IDs (\code{xmlflat.df$elemid.}) of the XML elements that are located on the provided path. Sub-elements of the elements on the search path are returned, too. \code{NULL}, if no elements where found.
#'
#' @details With \code{fxml_findPath()} it does not matter where exactly in the hierarchy of the XML document the path is found. If, for example, \code{path = c("tag1", "tag2")} then
#' the element with full XML path \code{<xml><testdoc><tag1><tag2>} would be found, too.
#'
#' Other \code{fxml_findPath...()} functions allow for different search modes:
#' \itemize{
#' \item \code{\link{fxml_findPathRoot}}: Search for path from the root node of the XML document downwards. Sub-elements are returned, too.
#' \item \code{\link{fxml_findPathFull}}: Search for exact path (always starting from the root node). No sub-elements returned, as they have a different path than the search path.
#' \item \code{\link{fxml_findPathBottom}}: Search for path from the bottom of the element hierarchy in the XML document.
#' }
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_findPathRoot}}, \code{\link{fxml_findPathFull}}, \code{\link{fxml_findPathBottom}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Find all XML elements with <data><record><field> in their XML path
#' path <- c("data", "record", "field")
#' fxml_findPath(xml.dataframe, path)
#'
#' # Find only those XML elements with <data><record><field> in their XML path that have the
#' # "name" attribute of the <field> element set to "Sex"
#' path <- c("data", "record", "field")
#' fxml_findPath(xml.dataframe, path, attr.only = list(field = c(name = "Sex")))
#' @export
fxml_findPath <- function(xmlflat.df, path, attr.only = NULL, attr.not = NULL) {
  if(isFlatXML(xmlflat.df)) {
    return(findPath(xmlflat.df, path, attr.only, attr.not, mode="any"))
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Finding XML elements
#' @description Finds all XML elements in an XML document that lie on a certain path. Search starts from the root node of the XML document. Sub-elements (children) of the elements on the search path are returned, too.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param path A character vector representing the path to be searched. Each element of the vector is a hierarchy level in the XML document. Example: \code{path = c("tag1", "tag2")}.
#' @param attr.only A list of named vectors representing attribute/value combinations the XML elements on the search path must match.
#' The name of an element in the list is the XML elment name to which the attribute belongs. The list element itself is a named vector.
#' The vector's elements represent different attributes (= the names of the vector elements) and their values (= vector elements).
#' Example: \code{attr.only = list(tag1 = c(attrib1 = "Value 1", attrib2 = "Value 2"), tag2 = c(attrib3 = "Value 3"))} will only find those elements which lie on a
#' path that includes \code{<tag1 attrib1 = "Value 1" attrib2 = "Value 2"><tag2 attrib3 = "Value 3">}.
#' @param attr.not A list of vectors representing attribute/value combinations the XML elements on the search path must not match to be included in the results. See argument \code{attr.only} for details on the composition.
#'
#' @return The IDs (\code{xmlflat.df$elemid.}) of the XML elements that are located on the provided path. Sub-elements of the elements on the search path are returned, too. \code{NULL}, if no elements where found.
#'
#' @details With \code{fxml_findPathRoot()}, the search always starts at the root node of the XML document. If, for example, \code{path = c("tag1", "tag2")} then
#' the element with full XML path \code{<xml><testdoc><tag1><tag2>} would not be found, only if search path were \code{c("xml", "testdoc", "tag1", "tag2")}
#'
#' Other \code{fxml_findPath...()} functions allow for different search modes:
#' \itemize{
#' \item \code{\link{fxml_findPath}}: Search for path anywhere in the XML document (not necessarily starting at the root node). Sub-elements are returned, too.
#' \item \code{\link{fxml_findPathFull}}: Search for exact path (always starting from the root node). No sub-elements returned, as they have a different path than the search path.
#' \item \code{\link{fxml_findPathBottom}}: Search for path from the bottom of the element hierarchy in the XML document.
#' }
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_findPath}}, \code{\link{fxml_findPathFull}}, \code{\link{fxml_findPathBottom}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Find all XML elements that have a path starting with <root><data><record><field>
#' path <- c("root", "data", "record", "field")
#' fxml_findPathRoot(xml.dataframe, path)
#'
#' # Find all XML elements that have a path starting with <root><data><record><field>, but only
#' # those which have the "name" attribute of the <field> element set to "Sex"
#' path <- c("root", "data", "record", "field")
#' fxml_findPathRoot(xml.dataframe, path, attr.only = list(field = c(name = "Sex")))
#' @export
fxml_findPathRoot <- function(xmlflat.df, path, attr.only = NULL, attr.not = NULL) {
  if(isFlatXML(xmlflat.df)) {
    return(findPath(xmlflat.df, path, attr.only, attr.not, mode="root"))
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}


#' @title Finding XML elements
#' @description Finds all XML elements in an XML document that lie on a certain path. The path of the found elements must match exactly the search path.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param path A character vector representing the path to be searched. Each element of the vector is a hierarchy level in the XML document. Example: \code{path = c("tag1", "tag2")}.
#' @param attr.only A list of named vectors representing attribute/value combinations the XML elements on the search path must match.
#' The name of an element in the list is the XML elment name to which the attribute belongs. The list element itself is a named vector.
#' The vector's elements represent different attributes (= the names of the vector elements) and their values (= vector elements).
#' Example: \code{attr.only = list(tag1 = c(attrib1 = "Value 1", attrib2 = "Value 2"), tag2 = c(attrib3 = "Value 3"))} will only find those elements which lie on a
#' path that includes \code{<tag1 attrib1 = "Value 1" attrib2 = "Value 2"><tag2 attrib3 = "Value 3">}.
#' @param attr.not A list of vectors representing attribute/value combinations the XML elements on the search path must not match to be included in the results. See argument \code{attr.only} for details on the composition.
#'
#' @return The IDs (\code{xmlflat.df$elemid.}) of the XML elements that are located on the provided path. Sub-elements of the elements on the search path are not returned as they have a different search path. \code{NULL}, if no elements where found.
#'
#' @details With \code{fxml_findPathRoot()}, the search always starts at the root node of the XML document. Only if an element has exactly the same path as the search path, it is returned as a result.
#' If, for example, \code{path = c("tag1", "tag2")} then the element with full XML path \code{<tag1><tag2><tag3>} would not be found, only if search path were \code{c("tag1", "tag2", "tag3")}.
#'
#' Other \code{fxml_findPath...()} functions allow for different search modes:
#' \itemize{
#' \item \code{\link{fxml_findPath}}: Search for path anywhere in the XML document (not necessarily starting at the root node). Sub-elements are returned, too.
#' \item \code{\link{fxml_findPathRoot}}: Search for path from the root node of the XML document downwards. Sub-elements are returned, too.
#' \item \code{\link{fxml_findPathBottom}}: Search for path from the bottom of the element hierarchy in the XML document.
#' }
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_findPath}}, \code{\link{fxml_findPathRoot}}, \code{\link{fxml_findPathBottom}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Find all XML elements that have the exact path <root><data><record>
#' path <- c("root", "data", "record")
#' fxml_findPathFull(xml.dataframe, path)
#' @export
fxml_findPathFull <- function(xmlflat.df, path, attr.only = NULL, attr.not = NULL) {
  if(isFlatXML(xmlflat.df)) {
    return(findPath(xmlflat.df, path, attr.only, attr.not, mode="full"))
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Finding XML elements
#' @description Finds all XML elements in an XML document that lie on a certain path. The path of the found elements must end with the provided search path.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param path A character vector representing the path to be searched. Each element of the vector is a hierarchy level in the XML document. Example: \code{path = c("tag1", "tag2")}.
#' @param attr.only A list of named vectors representing attribute/value combinations the XML elements on the search path must match.
#' The name of an element in the list is the XML elment name to which the attribute belongs. The list element itself is a named vector.
#' The vector's elements represent different attributes (= the names of the vector elements) and their values (= vector elements).
#' Example: \code{attr.only = list(tag1 = c(attrib1 = "Value 1", attrib2 = "Value 2"), tag2 = c(attrib3 = "Value 3"))} will only find those elements which lie on a
#' path that includes \code{<tag1 attrib1 = "Value 1" attrib2 = "Value 2"><tag2 attrib3 = "Value 3">}.
#' @param attr.not A list of vectors representing attribute/value combinations the XML elements on the search path must not match to be included in the results. See argument \code{attr.only} for details on the composition.
#'
#' @return The IDs (\code{xmlflat.df$elemid.}) of the XML elements that are located on the provided path. \code{NULL}, if no elements where found.
#'
#' @details With \code{fxml_findPathRoot()}, the search always starts at the bottom of the element hierarchy of the XML document. Only if the path of an elemends ends with the provided search path, it is returned as a result.
#' If, for example, \code{path = c("tag1", "tag2")} then the element with full XML path \code{<tag1><tag2><tag3>} would not be found, only if search path were \code{c("tag2", "tag3")}.
#'
#' Other \code{fxml_findPath...()} functions allow for different search modes:
#' \itemize{
#' \item \code{\link{fxml_findPath}}: Search for path anywhere in the XML document (not necessarily starting at the root node). Sub-elements are returned, too.
#' \item \code{\link{fxml_findPathRoot}}: Search for path from the root node of the XML document downwards. Sub-elements are returned, too.
#' \item \code{\link{fxml_findPathFull}}: Search for exact path (always starting from the root node). No sub-elements returned, as they have a different path than the search path.
#' }
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_findPath}}, \code{\link{fxml_findPathRoot}}, \code{\link{fxml_findPathFull}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Find all XML elements that have a path ending with <record><field>
#' path <- c("record", "field")
#' fxml_findPathBottom(xml.dataframe, path)
#'
#' # Find all XML elements that have a path ending with <record><field>, but only
#' # those which have the "name" attribute of the <field> element set to "Sex"
#' path <- c("record", "field")
#' fxml_findPathBottom(xml.dataframe, path, attr.only = list(field = c(name = "Sex")))
#' @export
fxml_findPathBottom <- function(xmlflat.df, path, attr.only = NULL, attr.not = NULL) {
  if(isFlatXML(xmlflat.df)) {
    return(findPath(xmlflat.df, path, attr.only, attr.not, mode="bottom"))
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Handling flat XML files
#' @description Returns the unique XML elements included in an XML document.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#'
#' @return A vector with all the names of the elements included in the XML document \code{xmlflat.df}. Every tag is only returned once, even if it occurs multiple times in the document. The return vector is empty (\code{NULL}) if no elements exist.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_getElement}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Identify the unique XML elements
#' fxml_getUniqueElements(xml.dataframe)
#' @export
fxml_getUniqueElements <- function(xmlflat.df) {
  if(isFlatXML(xmlflat.df)) {
      return(unique(xmlflat.df$elem.))
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Handling flat XML files
#' @description Returns the element name of an XML element.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return Name of the element identified by the ID (\code{xmlflat.df$elemid.}) \code{elemid}.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_getUniqueElements}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Get the XML element with ID 3 (xml.dataframe$elemid. ==  3)
#' fxml_getElement(xml.dataframe, 3)
#' @export
fxml_getElement <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) {
      return(xmlflat.df$elem.[is.na(xmlflat.df$attr.) & xmlflat.df$elemid. == elemid])
    }
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Extracting data from an XML document into a dataframe
#' @description Reads in data from an XML document and returns a dataframe.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param siblings.of ID of one of the XML elements that contain the data records. All data records need to be on the same hierarchical level as the XML element with this ID.
#' @param same.tag If \code{TRUE}, only elements of the same type (\code{xmlflat.df$elem.}) as the element \code{sibling.of} are considered as data records. If \code{FALSE},
#' \emph{all} elements on the same hierarchical level as the element \code{sibling.of} are considered to be data records.
#' @param attr.only A list of named vectors representing attribute/value combinations the data records must match.
#' The name of an element in the list is the XML element name to which the attribute belongs. The list element itself is a named vector.
#' The vector's elements represent different attributes (= the names of the vector elements) and their values (= vector elements).
#' Example: \code{attr.only = list(tag1 = c(attrib1 = "Value 1", attrib2 = "Value 2"), tag2 = c(attrib3 = "Value 3"))} will only include \code{tag1} elements of the form \code{<tag1 attrib1 = "Value 1" attrib2 = "Value 2">} and \code{tag2} elements of the form \code{<tag2 attrib3 = "Value 3">} as data records.
#' @param attr.not A list of vectors representing attribute/value combinations the XML elements must \emph{not} match to be considered as data records. See argument \code{attr.only} for details.
#' @param elem.or.attr Either \code{"elem"} or \code{"attr"}. Defines, if the names of the record fields (columns in the dataframe) are represented by the names (tags) of the respective XML elements
#' (the children of the elements on the same level as \code{siblings.of}) (\code{"elem"}) or if the field names are given by some attribute of those tags (\code{"attr"}).
#' @param col.attr If \code{elem.or.attr} is \code{"attr"} then \code{col.attr} specifies the name of the attribute that gives the record field / column names.
#' @param include.fields A character vector with the names of the fields that are to be included in the result dataframe. By default, all fields from the XML document are included.
#' @param exclude.fields A character vector with the names of the fields that should be excluded in the result dataframe. By default, no fields from the XML document are excluded.
#'
#' @return A dataframe with the data read in from the XML document.
#'
#' @details
#' Data that can be read in are either represented in this way:\cr \cr
#'   \code{<record>}\cr
#'     \code{<field1>Value of field1</field1>}\cr
#'     \code{<field2>Value of field2</field2>}\cr
#'     \code{<field3>Value of field3</field3>}\cr
#'   \code{</record>}\cr
#'   \code{...}\cr \cr
#' In this case \code{elem.or.attr} would need to be \code{"elem"} because the field names of the data records (\code{field1}, \code{field2}, \code{field3}) are the names of the elements.\cr \cr
#' Or, the XML data could also look like this: \cr \cr
#'   \code{<record>}\cr
#'     \code{<column name="field1">Value of field1</column>}\cr
#'     \code{<column name="field2">Value of field2</column>}\cr
#'     \code{<column name="field3">Value of field3</column>}\cr
#'   \code{</record>}\cr
#'   \code{...}\cr \cr
#' Here, the names of the fields are attributes, so \code{elem.or.attr} would need to be \code{"attr"} and \code{col.attr} would be set to
#' \code{"name"}, so \code{fxml_toDataframe()} knows where to look for the field/column names.\cr \cr
#' In any case, \code{siblings.of} would be the ID (\code{xmlflat.df$elemid.}) of one of the \code{<record>} elements.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_importXMLFlat}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Extract the data out of the XML document. The data records are on the same hierarchical level
#' # as element with ID 3 (xml.dataframe$elemid. ==  3).
#' # The field names are given in the "name" attribute of the children elements of element no. 3
#' # and its siblings
#' population.df <- fxml_toDataFrame(xml.dataframe, siblings.of=3, elem.or.attr="attr",
#' col.attr="name")
#' # Exclude the "Value Footnote" field from the returned dataframe
#' population.df <- fxml_toDataFrame(xml.dataframe, siblings.of=3, elem.or.attr="attr",
#' col.attr="name", exclude.fields=c("Value Footnote"))
#'
#'
#' # Load example file with soccer world cup data (data from
#' # https://www.fifa.com/fifa-tournaments/statistics-and-records/worldcup/index.html)
#' # and create flat dataframe
#' example2 <- system.file("soccer.xml", package="flatxml")
#' xml.dataframe2 <- fxml_importXMLFlat(example2)
#'
#' # Extract the data out of the XML document. The data records are on the same hierarchical level
#' # as element with ID 3 (xml.dataframe$elemid. ==  3). #' # The field names are given as the name
#' # of the children elements of element no. 3 and its siblings.
#' worldcups.df <- fxml_toDataFrame(xml.dataframe2, siblings.of=3, elem.or.attr="elem")
#' @export
fxml_toDataFrame <- function(xmlflat.df, siblings.of, same.tag = TRUE, attr.only = NULL, attr.not = NULL, elem.or.attr = "elem", col.attr = "", include.fields = NULL, exclude.fields = NULL) {
  if(isFlatXML(xmlflat.df)) {
    if(siblings.of %in% xmlflat.df$elemid.) {
      res <- data.frame()
      sibl <- fxml_getSiblings(xmlflat.df, siblings.of)
      # if(!is.null(sibl)) {
        nodes <- sort(c(sibl, siblings.of))
        for(i in 1:length(nodes)) {
          r<-c()
          if(same.tag == FALSE | xmlflat.df$elem.[is.na(xmlflat.df$attr.) & xmlflat.df$elemid. == nodes[i]] == xmlflat.df$elem.[is.na(xmlflat.df$attr.) & xmlflat.df$elemid. == siblings.of]) {
            if(ncol(res) > 0) {
              r <- rep(NA, ncol(res))
              names(r) <- names(res)
            }
            else r <- NULL

            chil <- fxml_getChildren(xmlflat.df, nodes[i])
            if(!is.null(chil)) {
              for(f in 1:length(chil)) {

                if(elem.or.attr == "elem") tag <- xmlflat.df$elem.[is.na(xmlflat.df$attr.) & xmlflat.df$elemid. == chil[f]]
                else tag <- fxml_getAttribute(xmlflat.df, chil[f], col.attr)

                # Determine if element is to be used
                use <- TRUE
                if(is.list(attr.only)) {
                  for(z in 1:length(attr.only)) {
                    for(n in 1:length(attr.only[[z]])) {
                      if(fxml_getElement(xmlflat.df, nodes[i]) == names(attr.only)[z]) {
                        if(fxml_getAttribute(xmlflat.df, nodes[i], names(attr.only[[z]][n])) != attr.only[[z]][n]) use <- FALSE
                        break
                      }
                    }
                  }
                }

                if(is.list(attr.not)) {
                  for(z in 1:length(attr.not)) {
                    for(n in 1:length(attr.not[[z]])) {
                      if(fxml_getElement(xmlflat.df, nodes[i]) == names(attr.not)[z]) {
                        if(fxml_getAttribute(xmlflat.df, nodes[i], names(attr.not[[z]][n])) == attr.not[[z]][n]) use <- FALSE
                        break
                      }
                    }
                  }
                }

                if(!is.null(include.fields)>0) {
                  use.field.incl <- FALSE
                  for(z in 1:length(include.fields)) {
                    if(tag == include.fields[z]) use.field.incl <- TRUE
                  }
                  use <- use & use.field.incl
                }

                if(!is.null(exclude.fields)) {
                  use.field.excl <- TRUE
                  for(z in 1:length(exclude.fields)) {
                    if(tag == exclude.fields[z]) {
                      use.field.excl <- FALSE
                    }
                  }
                  use <- use & use.field.excl
                }

                if(use) {
                  if(!(tag %in% names(res))) {
                    res <- cbind(res, rep(NA, nrow(res)), stringsAsFactors=FALSE)
                    names(res)[ncol(res)] <- tag
                    r <- append(r, fxml_getValue(xmlflat.df,chil[f]))
                    names(r)[NROW(r)] <- tag
                  }
                  else {
                    r[names(r)==tag] <- fxml_getValue(xmlflat.df,chil[f])
                  }
                }
              }
              res <- rbind(res, r, stringsAsFactors=FALSE)
              names(res) <- names(r)
              if(!is.na(fxml_getValue(xmlflat.df,nodes[i]))) rownames(res)[nrow(res)] <- fxml_getValue(xmlflat.df,nodes[i])
            }
          }
        }
      #}
      return(res)
    }
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}



#' @title Handling flat XML files
#' @description Returns summary information on an XML element.
#'
#' @param xmlflat.df A flat XML dataframe created with \code{\link{fxml_importXMLFlat}}.
#' @param elemid The ID of the XML element. The ID is the value of the \code{elemid.} field in the flat XML dataframe.
#'
#' @return A list with the following elements:
#' \itemize{
#' \item \code{value}: The value of the XML element; return value of the \code{\link{fxml_getValue}} function.
#' \item \code{path}: A vector representing the path from the root element of the XML element document to the current element. Each XML element on the path is represented by a element of the vector. The vector elements are the names of the XML elements on the path.
#' \item \code{depth.level}: The depth level (hierarchy level) of the XML element; return value of the \code{\link{fxml_getDepthLevel}} function.
#' \item \code{attributes}: A named vector with the attributes of the XML element (vector elements are the attributes' values, names of the vector elements are the attributes' names; return value of the \code{\link{fxml_getAttributesAll}} function.
#' \item \code{parent}: The parent of the XML element; return value of the \code{\link{fxml_getParent}} function.
#' \item \code{children}: The children of the XML element; return value of the \code{\link{fxml_getChildren}} function.
#' \item \code{siblings}: The siblings of the XML element; return value of the \code{\link{fxml_getSiblings}} function.
#' }
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @seealso \code{\link{fxml_getElement}}, \code{\link{fxml_getValue}}, \code{\link{fxml_getDepthLevel}}, \code{\link{fxml_getAttribute}}, \code{\link{fxml_getChildren}}, \code{\link{fxml_getParent}}, \code{\link{fxml_getSiblings}}
#' @examples
#' # Load example file with population data from United Nations Statistics Division
#' # and create flat dataframe
#' example <- system.file("worldpopulation.xml", package="flatxml")
#' xml.dataframe <- fxml_importXMLFlat(example)
#'
#' # Get all relevant information on the XML element with ID 4 (xml.dataframe$elemid. ==  4)
#' fxml_getElementInfo(xml.dataframe, 4)
#' @export
fxml_getElementInfo <- function(xmlflat.df, elemid) {
  if(isFlatXML(xmlflat.df)) {
    if(elemid %in% xmlflat.df$elemid.) {
      value <- fxml_getValue(xmlflat.df, elemid)
      path <- as.character(xmlflat.df[xmlflat.df$elemid. == elemid & is.na(xmlflat.df$attr.),5:(4+fxml_getDepthLevel(xmlflat.df, elemid))])
      rownames(path) <- NULL
      colnames(path) <- NULL
      depth.level <- fxml_getDepthLevel(xmlflat.df, elemid)
      attributes <- fxml_getAttributesAll(xmlflat.df, elemid)
      children <- fxml_getChildren(xmlflat.df, elemid)
      parent <- fxml_getParent(xmlflat.df, elemid)
      siblings <- fxml_getSiblings(xmlflat.df, elemid)
      return(list(value=value, path=path, depth.level=depth.level, attributes=attributes, parent=parent, children=children, siblings=siblings))
    }
    else stop("Invalid element ID.")
  }
  else stop(paste0("'", deparse(substitute(xmlflat.df)), "' is not a flat XML dataframe."))
}
