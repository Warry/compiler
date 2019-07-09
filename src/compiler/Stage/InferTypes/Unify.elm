module Stage.InferTypes.Unify exposing (unifyAllEquations)

import AST.Common.Type as Type exposing (Type)
import Error exposing (TypeError(..))
import Stage.InferTypes.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Stage.InferTypes.TypeEquation as TypeEquation exposing (TypeEquation)


{-| TODO document
-}
unifyAllEquations : List TypeEquation -> Result TypeError SubstitutionMap
unifyAllEquations equations =
    List.foldl
        (\equation substitutionMap ->
            let
                ( t1, t2 ) =
                    TypeEquation.unwrap equation
            in
            Result.andThen (unify t1 t2) substitutionMap
        )
        (Ok SubstitutionMap.empty)
        equations


unify : Type -> Type -> SubstitutionMap -> Result TypeError SubstitutionMap
unify t1 t2 substitutionMap =
    let
        debug = Debug.log "Unify" (t1, t2, substitutionMap)
    in
    if t1 == t2 then
        Ok substitutionMap

    else
        case ( t1, t2 ) of
            ( Type.Var id, _ ) ->
                unifyVariable id t2 substitutionMap

            ( _, Type.Var id ) ->
                unifyVariable id t1 substitutionMap

            ( Type.Function arg1 result1, Type.Function arg2 result2 ) ->
                unify result1 result2 substitutionMap
                    |> Result.andThen (unify arg1 arg2)

            _ ->
                Err (TypeMismatch t1 t2)


unifyVariable : Int -> Type -> SubstitutionMap -> Result TypeError SubstitutionMap
unifyVariable id type_ substitutionMap =
    let
        debug = Debug.log "unifyVariable" (id, type_, substitutionMap)
    in
    case SubstitutionMap.get id substitutionMap of
        Just typeForId ->
            let
                debudssg = Debug.log "unifyVariable 2" (typeForId)
            in
            unify typeForId type_ substitutionMap

        Nothing ->
            case
                Type.getVarId type_
                    |> Maybe.andThen (\id2 -> SubstitutionMap.get id2 substitutionMap)
                    |> Maybe.map (\typeForId2 -> unify (Type.Var id) typeForId2 substitutionMap)
            of
                Just newSubstitutionMap ->
                    let
                        debuddsdssg = Debug.log "unifyVariable 3" (newSubstitutionMap)
                    in
                    newSubstitutionMap

                Nothing ->
                    if occurs id type_ substitutionMap then
                        Err (OccursCheckFailed id type_)

                    else
                        let
                            debsdssg = Debug.log "unifyVariable 4" (id)
                        in

                        Ok (SubstitutionMap.insert id type_ substitutionMap)


occurs : Int -> Type -> SubstitutionMap -> Bool
occurs id type_ substitutionMap =
    let
        debug = Debug.log "Occurs" (id, type_, substitutionMap)
    in
    if type_ == Type.Var id then
        True

    else
        case
            Type.getVarId type_
                |> Maybe.andThen (\id2 -> SubstitutionMap.get id2 substitutionMap)
                |> Maybe.map (\typeForId2 -> occurs id typeForId2 substitutionMap)
        of
            Just doesOccur ->
                doesOccur

            Nothing ->
                case type_ of
                    Type.Function arg result ->
                        occurs id result substitutionMap
                            || occurs id arg substitutionMap

                    -- TODO potentially dangerous wildcard?
                    _ ->
                        False
