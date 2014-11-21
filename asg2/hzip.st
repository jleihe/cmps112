#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"Some functions used and/or adapted from classfiles"

nl := Character nl.

Character extend [
    isPrint [
    ^ (Character space <= self) & (self <= $~)
    ]
    visible [
	self isPrint ifTrue: [^ '$', self asString]
	ifFalse: [^ self asInteger printStringRadix: 8]
    ]
]

Object subclass: ZeroArray [
    |theArray|

    ZeroArray class >> new [
	self shouldNotImplement.
    ]

    ZeroArray class >> new: size [
	|result|
	result := super new.
	result init: size.
	^ result
    ]

    init: size [
	theArray := Array new: size.
    ]

    size [
	^ theArray size.
    ]

    at: index [
	^ theArray at: index + 1.
    ]

    at: index put: value [
	^ theArray at: index + 1 put: value.
    ]

    incr: index [
	(self at: index) isNil
	ifTrue: [ self at: index put: 0].
	self at: index put: (self at: index) + 1.
    ]

    keysAndValuesDo: aBlock [
	(0 to: (self size) - 1) do: [:index |
	    aBlock value: index value: (self at: index).
	]
    ]

]

Object subclass: Leaf [
    |char count|
    char [ ^ char ]
    count [ ^ count ]

    Leaf class >> new [
	self shouldNotImplement
    ]

    Leaf class >> new: aChar count: aCount [
	|result|
	result := super new.
	result setChar: aChar andCount: aCount.
	^result
    ]

    setChar: aChar andCount: aCount [
	char := aChar.
	count := aCount.
    ]

    <= other [
	^ (count < other count)
	| ((count = other count) & (char <= other char))
    ]

    printBase: aStream [
	^ aStream << self class << '(' << char << ',' << count
    ]

    printOn: aStream [
	(self printBase: aStream) << ')'.
    ]

    inorder: visitor prefix: string [
	visitor value: char value: string.
    ]
]



FileStream extend [
    catFile [
	|freqtable|
	freqtable := ZeroArray new: 256.
	[self atEnd not] whileTrue: [
	    |ordChar|
	    ordChar := self next asInteger.
	    freqtable incr: ordChar.
	].
	freqtable at: 255 put: 1.
	^ freqtable.
    ]
]

"------------------------------Start-----------------------------------"
"argument list has at least two and not more than 3 entries"
(Smalltalk arguments size > 1) & (Smalltalk arguments size < 4)
ifTrue: [
    |freqtable|
    filename := Smalltalk arguments at: 2.
    (((File name: filename) isFile ) | (filename = '-'))
    ifTrue: [
	filename = '-'
	ifTrue: [
	    freqtable := stdin catFile.
	]
	ifFalse: [
	    [
		|infile|
		infile := FileStream open: filename mode: 
		    FileStream read.
		freqtable := infile catFile.
		infile close.
	    ] on: SystemExceptions.FileError do: [:signal |
		|errno|
		errno := File errno.
		stdout flush.
		stderr << execname << ': ' << filename << ': '
			<< signal messageText << nl.
		stderr flush.
	    ]
	].
"--------Iterate over freqtable and enter data into priority queue-----"
	priority := SortedCollection new.
	freqtable keysAndValuesDo: [:key :value |
	    (value notNil and: [value > 0]) ifTrue: [
		priority add:  (Leaf new: key count: value).
	    ]
	].
	"Uncomment next line to see the sorted list"
	"priority inspect."
"------------------------Construct the tree----------------------------"
	
    ]
    ifFalse: [
	stderr << nl << 'WARNING: ' << filename asString.
	stderr << ' is not a valid input'.
	stderr << 'file.  Please try again.' << nl << nl.
    ].
]
ifFalse: [
    stderr << nl << 'WARNING: hzip.st requires input of the form' << nl.
    stderr << '     hzip.st -dtcu inputfile [outputfile]' << nl << nl.
].
