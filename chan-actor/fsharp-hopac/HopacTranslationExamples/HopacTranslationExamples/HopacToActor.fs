namespace simonjf.HopacToActor

module HopacToActor =
    module HopacModel =
        let undefined<'T> : 'T = failwith "Not implemented yet"
        type Job<'x> = Undefined
        let (>>=) (x: Job<'x>) (fn: 'x -> Job<'y>) = undefined :> Job<'y>
        let result (x: 'x): Job<'x> = undefined
        let start (job: Job<unit>): unit = undefined

        type Ch<'x> = UndefinedCh
        let ch (): Ch<'x> = undefined
        let give (ch: Ch<'x>) (x: 'x): Job<unit> = undefined
        let take (ch: Ch<'x>): Job<'x> = undefined


    module ActorModel =
        open HopacModel

        // Internal actor representation: A data type taking a channel to a thread
        // Parameterised over two types: 'a is the type of message accepted by the
        // actor, and 'x is the return type of the actor thread.
        type ActorThread<'a, 'x> = AT of (Ch<'a> -> Job<'x>)

        // Unwrapping function
        let unAT (AT f) = f

        // Construct a new actor thread (AT). Appeals to the Hopac Job bind operator: 
        //   Job<'a> -> ('a -> Job<'b>) -> Job<'b>

        // Take a channel aCh of type Ch<'a>.
        // Unwrap actor thread to get a function Ch<'a> -> Job<'x>.
        // Apply aCh to get a value of type Job<'x>.
        // Use Hopac bind, providing a function taking a value x of type 'x.
        // The function uses the function fn of type 'x -> ActorThread<'a, 'y>.
        // Use unAT again to unwrap this, applying the channel to get a function
        // returning a Job<'y>. Wrap in AT. Done.
        let (>>=) (a: ActorThread<'a, 'x>) (fn: 'x -> ActorThread<'a, 'y>): ActorThread<'a, 'y> =
            AT (fun aCh -> (unAT a) aCh >>=
                           fun x -> (unAT (fn x)) aCh)
        
        // Create an actor thread which results in a type of 'x
        let result (v: 'x): ActorThread<'a, 'x> =
            // Hopac return, wrap in an AT?
            AT (fun _ -> HopacModel.result v)

        // Take a message from the mailbox.
        // Really simple: the channel is a buffered queue, so just take something from it.
        let receive: ActorThread<'a, 'a> =
            AT (fun ch -> take ch)

        // Actor: represents an actor -- unforgeable address, can send to other actors, one
        // thread of execution
        type Actor<'a> = A of Ch<'a>
        let start (at: ActorThread<'a, unit>): Actor<'a> =
            // We represent the mailbox as a single channel.
            let ch = HopacModel.ch ()
            // Start the job -- give the new channel to the function in the actor thread,
            // spawn it off. Return the actor, identified by the new channel.
            HopacModel.start <| (unAT at) ch
            (A ch)

        // Return the current actor ID.
        let self: ActorThread<'a, Actor<'a>> =
            AT (fun ch -> HopacModel.result (A ch))

        // Sending is an asynchronous operation which, in a separate thread of execution,
        // puts a messages onto the message queue of this actor.
        // I'm a little confused as to why this is unit instead of Job<unit>, but alas.
        let send (a: Actor<'a>) (v: 'a): unit =
            let (A ch) = a
            HopacModel.give ch v |> HopacModel.start


