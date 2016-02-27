#lang scribble/base

@title{Research Protocol}

@section{Title of Research}

``The memorability of random passwords generated using Molis Hai''

@section{Name and Department}

John Clements, Department of Computer Science, Cal Poly

@section{Statement of purpose, benefits, and hypotheses}

Last year we devised a an algorithm called Molis Hai for generating random
passwords in a way that increases memorability without compromising
randomness. Our hypothesis is that these passwords are easier to
remember than passwords generated using a more traditional technique.

For instance, a traditionally generated 56-bit random password expressed
as a hexadecimal sequence might be:
@verbatim{b084f9b9 466bd7}
and the corresponding Molis Hai password would be:
@verbatim{deathe dear sheld prom}
We conjecture that the second one is more memorable than the first.

If the hypothesis holds, this should make it (more) possible to use
random passwords in settings where users are currently choosing
their own (generally terrible) passwords.

Our plan is to test the memorability of these passwords using a spaced
repetition tool that requires students to enter these passwords on
several occasions, spaced by several days each time, and to measure
the ability of the students to recall the password after the passage
of this time.

@section{Methods}

@subsection{Subjects}

Subjects will be students currently taking one or more specific classes at Cal
Poly (e.g., students currently enrolled in CPE430). Students are expected to
be a representative sample of the currently enrolled upper-level Computer Science
and Software Engineering students.

@subsection{Experimenters}

John Clements. Relevant qualifications: history of educational research.

@subsection{Materials}

At the beginning of approximately twenty labs during a single quarter, students
will be asked to spend approximately one minute using a computer-based
application designed to train them to type a particular password.  Here's
a screenshot of the tool in use:

@image[#:scale 0.4 "screenshot.png"]

Please note that no deception will be a part of this experiment: students will
know that they are participating in a study on password memorability.

The tool will collect information on the students' password attempts, and associate
them with anonymized ids. After the completion of the collection, the mapping from
student names to ids will be destroyed, ensuring that student attempts cannot be
linked to the student names.

We have no current plans to debrief students, though we do hope to share the
results with any interested students.

@section{Informed Consent Form}

See attached proposed Informed Consent form.

@section{Further Information}

For further information on the password generation algorithm,
refer to our ArXiv publication:

@itemlist[
@item{Clements, John. ``Generating 56-bit passwords using Markov Models (and Charles Dickens),''
available online at @url{http://arxiv.org/abs/1502.07786}}]
