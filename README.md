# Experimenter

This package can be used to run experiments and repetitions of these that are then evaluated and a
report generated. The user can specify parameters, which span a set of experiment instances that are
automatically run and continuously saved to the database, s.t. they can be resumed on the same or
other PCs. The design allows to run different experiment instances on different PCs at the same
time. Thus it efficiently distributes work.

All random generators are repeated for the experiments. Thus, all (evaluation) replications use the
same standard number generator, Even after a crash and restart of the experiment on a different
computer.

At the end of an experiment a LaTeX report can be generated according to the user specifications.
E.g. one might be interested in the mean and standard deviation of an experiment that rolls a dice.
This can be specified by:

    [ Mean   OverExperimentRepetitions (Of "draw") `Named` "Mean Repetitions"
    , StdDev OverExperimentRepetitions (Of "draw") `Named` "StdDev Repetitions"
    ]

The part left of the `Named` tag specifies the metric and right of it is the name that will be used
in the report.

Each experiment can have a preparation phase, e.g. a learning phase for machine learning algorithms,
a warm-up phase, e.g. a queuing system that needs to be filled and get into the steady with the same
properties of the evaluation phase. The later is the one used for generating report of user defined
metrics. For instance, the user can define the dice draw like this:

    let result = StepResult "draw" Nothing (fromIntegral $ 1 + nr `mod` 6)
    in ...

The package provides a simple interface class `ExperimentDef a`, which is all that is needed to run
experiments in. In the simplest form this amounts to a datatype that holds the experiment state and
a function that implements one step of the experiment:

    -- | Data type for state
    data Dice =
      Dice
        StdGen -- ^ Random number generator
      deriving (Show, Generic)

    -- | NFData and Serialize instances
    ...

    -- | Experiment definition
    instance ExperimentDef Dice where
      type ExpM Dice = IO
      type InputValue Dice = ()      -- ^ No Input value
      type InputState Dice = ()      -- ^ No Input state is generated when generating the input value
      type Serializable Dice = Dice  -- ^ Serialisation type is the same. One can use

      -- ^ One step, that is one dice draw.
      runStep _ (Dice g) _ _ =
        let (nr, g') = next g
            result = StepResult "draw" Nothing (fromIntegral $ 1 + nr `mod` 6)
        in return ([result], Dice g')


Here the state is saved in the type `Dice`, no further input information is used for each step. In
an experiment of a production system this might be the demand of the current period, which should be
the same over all instances of the experiment to ensure every instance has the same properties. The
function `runStep` makes one draw of a `6` sided dice and returns the result as well as the new
state.

The experiment can then be configured like this:

    setup :: MkExperimentSetting a
    setup _ =
      ExperimentSetting
        { _experimentBaseName             = "dice param experiment" -- ^ Gives the experiment a name.
        , _experimentInfoParameters       = []                      -- ^ For storing information which only effects the report.
        , _experimentRepetitions          = 2                       -- ^ How often to repeat the whole experiment (including the preperation phase).
        , _preparationSteps               = 0                       -- ^ How many steps to execute for the preperation phase.
        , _evaluationWarmUpSteps          = 1000                    -- ^ How many steps to execute for the warm-up phase.
        , _evaluationSteps                = 10000                   -- ^ How many steps to execute for the evaluation phase.
        , _evaluationReplications         = 1                       -- ^ How often to execute the evaluation for each experiment repetition.
        , _evaluationMaxStepsBetweenSaves = Just 100000             -- ^ Specify after how many steps the data will be saved. `Nothing` adaptively chooses a sensible value.
        }

See [Dice.hs](examples/Dice.hs) for a full implementation, where the sides of the dices are
variable over the experiment.


# Ideas and TODOs

- Time of experiment run as step result.
- Nice Reports
- Html Results?
- Statistical measures for reports


# Help

Any help is warmly welcomed. Please feel free to start coding, submit pull requests or contact me :)
