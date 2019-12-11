from .tk import tk
def l_setLinkedStates(widget, states):
    '''Modify States of a Plot that are Linked in Loon's Standard Linking Model

    Description:
        Loon's standard linking model is based on three levels, the 
        `linkingGroup` and `linkingKey` states and the *used linkable states*.
        See the details below.
    
    Args:
        widget: widget path as a string or as an loon class 
        states: used linkable state names, see in details below
    
    Details:
        Loon's standard linking model is based on two states, 
        `linkingGroup` and `linkingKey`. The full capabilities of the 
        standard linking model are described here. However, setting the 
        `linkingGroup` states for two or more displays to the same string is 
        generally all that is needed for linking displays that plot data from the 
        same data frame. Changing the linking group of a display is also the only 
        linking-related action available on the analysis inspectors.

        The first linking level is as follows: loon's displays are linked if they 
        share the same string in their `linkingGroup` state. The default 
        linking group 'none' is a keyword and leaves a display un-linked.

        The second linking level is as follows. All `n`-dimensional states can
        be linked between displays. We call these states *linkable. Further, 
        only linkable states with the same name can be linked between displays. One
        consequence of this *shared state name* rule is that, with the 
        standard linking model, the `linewidth` state of a serialaxes display 
        cannot be linked with the `size` state of a scatterplot display. Also,
        each display maintains a list that defines which of its linkable states 
        should be used for linking; we call these states the *used linkable* 
        states. The default used linkable states are as follows
        <table>
        <tr><th>Display         <th> Default *used linkable* states
        <tr><td>scatterplot     <td> `selected`,`color`, `active`, `size`
        <tr><td>histogram       <td> `selected`,`color`, `active`
        <tr><td>serialaxes      <td> `selected`,`color`, `active`
        <tr><td>graph           <td> `selected`,`color`, `active`, `size`
        </table>

        If any two displays are set to be linked (i.e. they share the same linking 
        group) then the intersection of their *used linkable* states are 
        actually linked.

        The third linking level is as follows. Every display has a 
        `n`-dimensional `linkingKey` state. Hence, every data point has 
        an associated linking key. Data points between linked plots are linked if 
        they share the same linking key.
    
    @namespace loon.l_setLinkedStates
    '''
    if(not isinstance(widget, str)):
        widget = widget.plot
    return tk.tk.call(widget, 'setLinkedStates', states)



def l_getLinkedStates(widget):
    '''Query the States that are Linked with Loon's Standard Linking Model

    Description:
        Loon's standard linking model is based on three levels, the 
        `linkingGroup` and `linkingKey` states and the *used linkable states*.
        See the details in the documentation for l_setLinkedStates.
    
    Args:
        widget: widget path as a string or as an loon class 
    
    Return:
        list with state names that are linked states
    
    SeeAlso:
        l_setLinkedStates

    @namespace loon.l_getLinkedStates
    '''
    if(not isinstance(widget,str)):
        widget = widget.plot
    return list(tk.tk.call(widget, 'getLinkedStates'))

