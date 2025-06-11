def shift_correlations(df, shift_from=0, shift_to=0, shift_step=1, column="pegel", dropna=True):
    """
    Calculate a series of correlations of the given column with all other columns in the dataframe after shifting it until the given timestep. 
    Returns a dataframe with the correlations for each shift.
    To find the shift with the highest correlation, use the idxmax() function on the column you are looking for, example: shift_correlations(df, shift_from=-72, shift_to=0, column="pegel")["precipitation"].idxmax()
    """
    corr_df = pd.DataFrame(columns=["shift"] + df.columns.tolist())
    corr_df.set_index("shift", inplace=True)
    # calculate correlation for each shift
    for index in range(shift_from, shift_to+1, shift_step):
        df_shifted = df.copy()
        df_shifted[column] = df_shifted[column].shift(index) # shift
        if dropna:
            df_shifted = df_shifted.dropna()  # drop rows with NaN values after shifting
        corr = df_shifted.corr()
        # set row shift value
        corr_df.loc[index, "shift"] = index
        # add row to correlation dataframe
        for c in df_shifted.columns.tolist():
            corr_df.loc[index, c] = float(corr[column][c])
    return corr_df
