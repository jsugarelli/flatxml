# flatXML - Tools for Working with XML Files as R Dataframes

## Overview
`flatxml` provides functions to easily deal with XML files. When parsing an XML document with `fxml_importXMLFlat`, `flatxml` produces a special dataframe that is 'flat' by its very nature but contains all necessary information about the hierarchical structure of the underlying XML document (for details on the dataframe see the reference for the `fxml_importXMLFlat` function).

`flatxml` offers a set of functions to work with this dataframe.

Apart from representing the XML document in a dataframe structure, there is yet another way in which `flatxml` relates to dataframes: the `fxml_toDataFrame` and `fxml_toXML` functions can be used convert XML data to dataframes and vice versa.

Each XML element, for example `<tag attribute="some value">Here is some text</tag>` has certain characteristics that can be accessed via the `flatxml` interface functions, after an XML document has been imported with `fxml_importXMLFlat`. These characteristics are:

* `value`: The (text) value of the element, `"Here is some text"` in the example above
* `attributes`: The XML attributes of the element, `attribute` with its value `"some value"` in the example above
* `children`: The elements on the next lower hierarchical level
* `parent`: The element of the next higher hierarchical level, i.e. the element to which the current element is a child
* `siblings`: The elements on the same hierarchical level as the current element



## Structure of the flatxml interface
The `flatxml` interface to access these characteristics follows a simple logic: For each of the characteristics there are typically three functions available:

* `fxml_has...()`: Determines if the current XML element has (at least one instance of) the characteristic
* `fxml_num...()`: Returns the number of the characteristics of the current XML (e.g. the number of children elements)
* `fxml_get...()`: Returns (the IDs of) the respective characteristics of the current XML element (e.g. the children of the current element)



## Functions to access the characteristics of an XML element

For values:

* `fxml_hasValue`
* `fxml_getValue`

For attributes:

* `fxml_hasAttributes`
* `fxml_numAttributes`
* `fxml_getAttribute` (note: no plural 's'!)
* `fxml_getAttributesAll` (get all attributes instead of a specific one)

For children:

* `fxml_hasChildren`
* `fxml_numChildren`
* `fxml_getChildren`

For parents:

* `fxml_hasParent`
* `fxml_getParent`

For siblings:

* `fxml_hasSiblings`
* `fxml_numSiblings`
* `fxml_getSiblings`



## Functions for searching in the XML document

* `fxml_findPath` (search anywhere in the path to an XML element)
* `fxml_findPathFull` (find an element based on its complete path)
* `fxml_findPathRoot` (search in the path to an XML element starting at the top element [root node])
* `fxml_findPathBottom` (search in the path to an XML element starting at the lowest hierarchical level)



## Functions for converting between XML and dataframe

* `fxml_toDataFrame` (converts a (flattened) XML document to a dataframe)
* `fxml_toXML` (converts a dataframe to an XML document)



## Other functions

* `fxml_getElement` (name on an XML element (the `tag` in `<tag>…</tag>`)
* `fxml_getUniqueElements` (unique XML elements in the document)
* `fxml_getElementInfo` (all relevant information on an XML element (children, siblings, etc.)
* `fxml_getDepthLevel` (level of an element in the hierarchy of the XML document)
