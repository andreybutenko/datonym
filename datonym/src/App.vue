<template>
  <div id="app">
    <NavBar />
    <router-view/>
  </div>
</template>

<script>
  import NavBar from '@/components/NavBar.vue';

  export default {
    name: 'App',
    components: {
      NavBar,
    },
  };
</script>

<style lang="scss">
  .container.is-spaced {
    padding: 1rem 0;
  }
</style>
