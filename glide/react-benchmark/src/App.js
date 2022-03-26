import React, { useState, useEffect } from "react";

const maxN = 1000;

function Checky({ setN, ix, ckd, n }) {
  return <input checked={ckd} onChange={()=>{setN(n+1)}} type="checkbox"></input>;
}
// function Checky({ setN, ckd }) {
//   return <span>{(ckd ? 'y' : 'n')}</span>;
// }

function App() {
  const [state, setState] = React.useState({ rate: 4.0, ixLow: 0, ixHigh: 0, prevInternalTime:0.0, prevTime: null });
  const [n, setN] = useState(0);
  const [rate] = useState(4.0);
  const requestRef = React.useRef();

  const animate = () => {
    const time = Date.now() / 1000.0;
    if (state.prevTime === null) {
      setState({ rate: 4.0, ixLow: 0, ixHigh: 0, prevInternalTime:0.0, prevTime: time });
    } else {
      const elapsedTime = time - state.prevTime;
      const internalTime = state.prevInternalTime + elapsedTime * state.rate;
      const lowerBound = Math.max(0, Math.floor(internalTime));
      const diffUp = Math.floor(
        Math.min(5.0, Math.sin(internalTime * 0.5 * Math.PI + 1.0) * 40.0)
      );
      const upperBound = lowerBound + diffUp;
     
      setState({
        ixLow: lowerBound,
        ixHigh: upperBound,
        prevTime: time,
        prevInternalTime: internalTime,
      });
    }
    requestRef.current = requestAnimationFrame(animate);
  };

  React.useEffect(() => {
    requestRef.current = requestAnimationFrame(animate);
    return () => cancelAnimationFrame(requestRef.current);
  }, []);
  return (
    <div>
      <div>{n}</div>
      {Array.from(Array(maxN)).map((_, i) => (
        <Checky
          key={i}
          ix={i}
          n={n}
          ckd={i >= state.ixLow && i < state.ixHigh}
          setN={setN}
        ></Checky>
      ) ) }
    </div>
  );
}

export default App;
