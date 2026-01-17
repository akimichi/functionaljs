"use strict";

import { Maybe, Either } from './monad';

// Generic Monad interface for transformers
interface MonadOps<M> {
  unit: <A>(value: A) => M;
  flatMap: <A>(ma: M) => <B>(f: (a: A) => M) => M;
  self?: () => MonadOps<M>;
}

/**
 * MaybeT - Maybe Monad Transformer
 *
 * ~~~haskell
 * newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
 * ~~~
 */
export const MaybeT = {
  fail: <M>(_: any) => (Monad: MonadOps<M>): M => {
    return Monad.unit(Maybe.nothing());
  },

  unit: <M, A>(x: A) => (Monad: MonadOps<M>): M => {
    return Monad.unit(Maybe.unit(x));
  },

  /**
   * flatMap :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
   */
  flatMap: <M, A>(maybeT: M) => <B>(f: (a: A) => M) => (Monad: MonadOps<M>): M => {
    return Monad.flatMap(maybeT)((maybeInstance: any) => {
      return Maybe.match(maybeInstance, {
        nothing: () => {
          return Monad.unit(Maybe.nothing());
        },
        just: (v: A) => {
          return f(v);
        }
      });
    });
  },

  /**
   * lift :: Monad m => m a -> MaybeT m a
   */
  lift: <M>(m: M, Monad: MonadOps<M>): M => {
    return Monad.flatMap(m)((x: any) => {
      return Monad.unit(Maybe.unit(x));
    });
  }
};

/**
 * ErrorT (EitherT) - Either Monad Transformer
 *
 * ~~~haskell
 * newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }
 * ~~~
 */
export const ErrorT = {
  unit: <M, A>(x: A) => (Monad: MonadOps<M>): M => {
    return Monad.unit(Either.unit(x));
  },

  /**
   * flatMap :: ErrorT e m a -> (a -> ErrorT e m b) -> ErrorT e m b
   */
  flatMap: <M, E, A>(errorT: M) => <B>(f: (a: A) => M) => (Monad: MonadOps<M>): M => {
    return Monad.flatMap(errorT)((eitherInstance: any) => {
      return Either.match(eitherInstance, {
        left: (l: E) => {
          return Monad.unit(Either.left(l));
        },
        right: (r: A) => {
          return f(r);
        }
      });
    });
  },

  throwError: <M, E>(x: E) => (Monad: MonadOps<M>): M => {
    return Monad.unit(Either.left(x));
  },

  catchError: <M, E, A>(m: M) => (f: (e: E) => M) => (Monad: MonadOps<M>): M => {
    return Monad.flatMap(m)((eitherInstance: any) => {
      return Either.match(eitherInstance, {
        left: (l: E) => {
          return f(l);
        },
        right: (r: A) => {
          return Monad.unit(Either.right(r));
        }
      });
    });
  }
};

// Main export for compatibility
export const MonadTransformer = {
  MaybeT,
  ErrorT
};

export default MonadTransformer;
