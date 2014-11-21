#! /apps/smalltalk-3.2.5/gst --kernel-directory /apps/smalltalk-3.2.5/kernel/ -f

nl := Character nl.

stdout << 'arguments: ' << Smalltalk arguments << nl.

"""
stdout << 'hello' << nl.
Object subclass: Account [
    | balance |
    <comment:
    'I represent a place to deposit and withdraw money'>
]

Account extend [
    <comment:
    'I represent a place to withdraw money that has been deposited'>
]

Account class extend [
    new [
	| r |
	<category: 'instance creation'>
	r := super new.
	r init.
	^r
    ]
]

Account new.
"""
