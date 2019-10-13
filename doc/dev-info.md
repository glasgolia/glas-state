#glas-state development info
## This document is out of date
## Terms
* `Machine-def`
    This is the map structure that defines a statechart.
*  `Machine`
    This is an instance of a statechart, normally defined
    by a `Machine-def`
* `value`
    This is a map representing the value of a statechart
* `context`
    This is the extended state of a statechart
* `chart-state`
    This is the complete state for a statechart instance.
    This contains the `value`, the set of active `activities` 
    and the `context`
* `transition-state`
    This is the result of a transition.
    This contains the new value, the active activities and all actions to be taken.
    
        
## Structures
### Transition state
```
{:value <the-new-value>
 :actions [<actions>...]
}
```
### chart-state
```
{:value <the-current-value of the statechart>
 :activities #{The set of active activities}
 :context <The current context object>
```
