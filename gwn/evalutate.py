def calc_metrics(df):
    """
    calculate metrics (e.g., rmse and nse)
    :param df:[pd dataframe] dataframe of observations and predictions for
    one reach. dataframe must have columns "obs" and "pred"
    :return: [pd Series] various evaluation metrics (e.g., rmse and nse)
    """
    obs = df["obs"].values
    pred = df["pred"].values
    if len(obs) > 10:
        metrics = {
            "rmse": rmse(obs, pred).numpy(),
            "nse": nse(obs, pred).numpy(),
            "rmse_top10": percentile_metric(
                obs, pred, rmse, 90, less_than=False
            ).numpy(),
            "rmse_bot10": percentile_metric(
                obs, pred, rmse, 10, less_than=True
            ).numpy(),
            "rmse_logged": rmse_logged(obs, pred).numpy(),
            "nse_top10": percentile_metric(
                obs, pred, nse, 90, less_than=False
            ).numpy(),
            "nse_bot10": percentile_metric(
                obs, pred, nse, 10, less_than=True
            ).numpy(),
            "nse_logged": nse_logged(obs, pred).numpy(),
            "kge": kge(obs, pred).numpy(),
            "rmse_logged": rmse_logged(obs, pred),
            "nse_top10": percentile_metric(obs, pred, nse, 90, less_than=False),
            "nse_bot10": percentile_metric(obs, pred, nse, 10, less_than=True),
            "nse_logged": nse_logged(obs, pred),
        }

    else:
        metrics = {
            "rmse": np.nan,
            "nse": np.nan,
            "rmse_top10": np.nan,
            "rmse_bot10": np.nan,
            "rmse_logged": np.nan,
            "nse_top10": np.nan,
            "nse_bot10": np.nan,
            "nse_logged": np.nan,
            "kge": np.nan,
        }
    return pd.Series(metrics)

def plot_ts(pred_file, obs_file, variable, out_file):
    combined = fmt_preds_obs(pred_file, obs_file, variable)
    combined = combined.droplevel("seg_id_nat")
    ax = combined.plot(alpha=0.65)
    plt.tight_layout()
    plt.savefig(out_file)